(in-package #:jsonrpc)

(define-condition eof (error) ())

(defclass tcp-transport ()
  ((host :initarg :host
         :initform "127.0.0.1")
   (port :initarg :port
         :initform (random-port))
   (securep :initarg :securep
            :initform nil)))

(defclass tcp-server (tcp-transport server) ())
(defclass tcp-client (tcp-transport client) ())

(defmethod initialize-instance :after ((transport tcp-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (securep host port) transport
    (when url
      (let ((uri (quri:uri url)))
        (unless (quri:uri-http-p uri)
          (error "Only http or https are supported for tcp-transport (specified ~S)" (quri:uri-scheme uri)))
        (setf securep (equalp (quri:uri-scheme uri) "https")
              host (quri:uri-host uri)
              port (quri:uri-port uri))))
    transport))

(defmethod transport-finalize-connection ((transport tcp-transport) (connection connection))
  "Normally called on finalizer of reader thread."
  (with-slots (reader processor io) connection
    (when (bt:thread-alive-p processor)
      (bt:destroy-thread processor))
    (setf reader nil
          processor nil)
    (close io)
    ))

(defun transport-tcp-start-reader (connection &key payload-reader processor (name "reader"))
  (setf (slot-value connection 'processor) processor)
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (loop for payload = (funcall payload-reader connection)
             while payload
             do
               (chanl:send (slot-value connection 'inbox) payload)
               (connection-notify-ready connection))
       
       ;;(connection-finalize connection)
       ;;(with-slots (transport) connection
       (transport-finalize-connection (slot-value connection 'transport) connection)
       ))
   :name name
   ))

(defmethod transport-connect ((transport tcp-client))
  (with-slots (host port securep) transport
    (let ((io (usocket:socket-stream
               (usocket:socket-connect host port :element-type '(unsigned-byte 8)))))
      (when securep
        (setf io (cl+ssl:make-ssl-client-stream io :hostname host)))

      (let ((connection (make-instance 'connection :io io :transport transport))
            (bt:*default-special-bindings* (append bt:*default-special-bindings*
                                                   `((*standard-output* . ,*standard-output*)
                                                     (*error-output* . ,*error-output*)))))
        (let ((processor (connection-processor connection :name "jsownrpc/tcp-client/processor" :payload-writer #'%payload-writer-tcp)))
          (transport-tcp-start-reader connection
                                      :name "jsownrpc/tcp-client/reader"
                                      :processor processor
                                      :payload-reader #'%payload-reader-tcp))

        connection))))

(defmethod transport-listen ((transport tcp-server))
  (with-slots (listener host port) transport
    (when listener (error 'transport-already-listening))
    (setf listener
          (bt:make-thread
           (lambda ()
             (usocket:with-socket-listener (server-socket host port :reuse-address t :element-type '(unsigned-byte 8))
               (let ((bt:*default-special-bindings* (append bt:*default-special-bindings*
                                                            `((*standard-output* . ,*standard-output*)
                                                              (*error-output* . ,*error-output*)))))

                 (unwind-protect
                      (loop
                         (usocket:wait-for-input (list server-socket) :timeout 10)
                         (when (member (usocket:socket-state server-socket) '(:read :read-write))
                           (let* ((socket (usocket:socket-accept server-socket))
                                  (connection (make-instance 'connection
                                                             :io (usocket:socket-stream socket)
                                                             :transport transport)))
                             (let ((processor (connection-processor connection :name "jsownrpc/tcp-server/processor" :payload-writer #'%payload-writer-tcp)))
                               (transport-tcp-start-reader connection
                                                           :name "jsownrpc/tcp-server/reader"
                                                           :processor processor
                                                           :payload-reader #'%payload-reader-tcp)

                               ;; delegate transport to manage connection
                               (emit :accepted transport connection)
                               ))))

                   ;; finalize listener
                   (emit :end-of-listening transport)
                   ))))
           :name "jsonrpc/transport/tcp listener"
           ))))

;;;; internal

(defun %payload-writer-tcp (connection payload)
  (let* ((json (jsown:to-json payload))
         (body (string-to-utf-8-bytes json))
         (io (slot-value connection 'io)))
    (write-sequence
     (string-to-utf-8-bytes
      (format nil
              "Content-Length: ~A~C~C~:*~:*~C~C"
              (length body)
              #\Return
              #\Newline))
     io)
    (write-sequence body io)
    (force-output io)))

(defun %payload-reader-tcp (connection &aux (transport (slot-value connection 'transport)))
  (handler-case
      (let* ((io (slot-value connection 'io))
             (headers (%read-headers io))
             (length (ignore-errors (parse-integer (gethash "content-length" headers)))))
        (when length
          (let ((body (make-array length :element-type '(unsigned-byte 8))))
            (read-sequence body io)
            ;; TODO: error handlin
            ;;;; jsown cause bug. body to be ["RESPONSE-RESULT", "PAYLOAD"]
            (message-json-to-payload (utf-8-bytes-to-string body) :need-jsonrpc-field-p (slot-value transport 'need-jsonrpc-field-p)))))
    (eof () nil)))

(defun %read-headers (io)
  (check-type io stream)
  (let (header-field (headers (make-hash-table :test 'equal)))
    (tagbody
     read-header-field
       (let ((buffer (fast-io:make-output-buffer)))
         ;; The last of headers
         (let ((byte (read-byte io nil 0)))
           (cond
             ((= byte (char-code #\Return))
              (progn
                (assert (= (read-byte io nil 0) (char-code #\Linefeed)))
                (go finish)))
             ((= byte 0)
              (go eof))
             (t
              (fast-write-byte byte buffer))))
         (loop for byte of-type (unsigned-byte 8) = (read-byte io nil 0)
            if (= byte (char-code #\:))
            do (setf header-field
                     (string-downcase
                      (map 'string #'code-char (fast-io:finish-output-buffer buffer))))
              (go read-header-value)
            else if (= byte 0)
            do (go eof)
            else if (= byte (char-code #\Return))
            do (go read-lf)
            else
            do (fast-write-byte byte buffer)))

     read-header-value
       (let ((buffer (fast-io:make-output-buffer)))
         (let ((byte (read-byte io nil 0)))
           (unless (= byte (char-code #\Space))
             (fast-io:fast-write-byte byte buffer)))
         (loop for byte of-type (unsigned-byte 8) = (read-byte io nil 0)
            if (= byte 0)
            do (go eof)
            else if (= byte (char-code #\Return))
            ;; FIXME: The same header field can be found and should be concatenated into the same value
            do (setf (gethash header-field headers)
                     (map 'string #'code-char (fast-io:finish-output-buffer buffer)))
              (go read-lf)
            else
            do (fast-write-byte byte buffer)
            until (= byte (char-code #\Return))))

     read-lf
       (let ((byte (read-byte io nil 0)))
         (assert (= byte (char-code #\Linefeed)))
         (go read-header-field))

     eof
       (error 'eof)

     finish)

    headers))
