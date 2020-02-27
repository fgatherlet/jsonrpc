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
  (when url
    (let ((uri (quri:uri url)))
      (unless (quri:uri-http-p uri)
        (error "Only http or https are supported for tcp-transport (specified ~S)" (quri:uri-scheme uri)))
      (setf (slot-value transport 'securep) (equalp (quri:uri-scheme uri) "https"))
      (setf (slot-value transport 'host) (quri:uri-host uri))
      (setf (slot-value transport 'port) (quri:uri-port uri))))
  transport)

(defmethod start ((transport tcp-server))
  (setf (slot-value transport 'thread)
        (bt:make-thread
         (lambda ()
           (usocket:with-socket-listener (server-socket (slot-value transport 'host)
                                                        (slot-value transport 'port)
                                                        :reuse-address t
                                                        :element-type '(unsigned-byte 8))
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
                                                           :transport transport
                                                           ))
                                )
                           
                           (connection-prepare-destruction-hook connection)
                           
                           (setf (slot-value connection 'threads)
                                 (list
                                  (bt:make-thread
                                   (lambda () (connection-read-loop connection :payload-reader #'payload-reader-tcp))
                                   :name "jsonrpc/transport/tcp reader")
                                  
                                  (bt:make-thread
                                   (lambda () (connection-process-loop connection :payload-writer #'payload-writer-tcp))
                                   :name "jsonrpc/transport/tcp processer")))
                           ;;(let ((thread
                           ;;                ;; ------------------------------
                           ;;                ;; handle inbox || handle outbox
                           ;;                (bt:make-thread
                           ;;                 (lambda () (connection-process-loop connection :payload-writer #'payload-writer-tcp))
                           ;;                 :name "jsonrpc/transport/tcp processing"
                           ;;                 :initial-bindings
                           ;;                 `((*standard-output* . ,*standard-output*)
                           ;;                   (*error-output* . ,*error-output*)))))
                           ;;            (unwind-protect
                           ;;                ;; ------------------------------
                           ;;                ;; read and (enqueue request to inbox) or (dispatch response)
                           ;;                 (connection-read-loop connection :payload-reader #'payload-reader-tcp)
                           ;;             (finish-output (slot-value connection 'io))
                           ;;             (usocket:socket-close socket)
                           ;;             (bt:destroy-thread thread)
                           ;;             (emit :close connection))))
                           ;;       :name "jsonrpc/transport/tcp reading"))
                           ;;
                           ;;(push thread client-threads)
                           (push connection (slot-value transport 'connections)))))
                 
                 ;;(mapc #'bt:destroy-thread client-threads)
                 (mapc #'connection-destroy (slot-value transport 'connections))
                 ))))
         :name "jsonrpc/transport/tcp listener"
         )))

(defmethod start ((transport tcp-client))
  (let ((io (usocket:socket-stream
             (usocket:socket-connect (slot-value transport 'host)
                                     (slot-value transport 'port)
                                     :element-type '(unsigned-byte 8)))))
    (when (slot-value transport 'securep)
      (setf io (cl+ssl:make-ssl-client-stream io :hostname (slot-value transport 'host))))
    
    (let ((connection (make-instance 'connection :io io :transport transport))
          (bt:*default-special-bindings* (append bt:*default-special-bindings*
                                                 `((*standard-output* . ,*standard-output*)
                                                   (*error-output* . ,*error-output*)))))
      (setf (slot-value connection 'threads)
            (list
             ;; ------------------------------
             ;; handle inbox || handle outbox
             (bt:make-thread
              (lambda ()
                (connection-process-loop connection :payload-writer #'payload-writer-tcp))
              :name "jsonrpc/transport/tcp processing")
             
             ;; ------------------------------
             ;; read and (enqueue request to inbox) or (dispatch response)
             (bt:make-thread
              (lambda ()
                (connection-read-loop connection :payload-reader #'payload-reader-tcp)
                :name "jsonrpc/transport/tcp reading"))
             ))
      
      (push connection (slot-value transport 'connections))
      
      connection)))

;;;; internal

(defun payload-writer-tcp (connection payload)
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

(defun payload-reader-tcp (connection &aux (transport (slot-value connection 'transport)))
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
