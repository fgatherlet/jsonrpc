(in-package #:jsonrpc)

(defclass websocket-transport (transport)
  ((host :initarg :host
         :initform "127.0.0.1")
   (port :initarg :port
         :initform (random-port))
   (path :initarg :path
         :initform "/")
   (securep :initarg :securep
            :initform nil)
   (debug :initarg :debug
          :initform t)))

(defmethod initialize-instance :after ((transport websocket-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (when url
    (let ((uri (quri:uri url)))
      (unless (member (quri:uri-scheme uri) '("ws" "wss") :test #'equalp)
        (error "Only ws or wss are supported for websocket-transport (specified ~S)" (quri:uri-scheme uri)))
      (setf (slot-value transport 'securep) (equalp (quri:uri-scheme uri) "wss"))
      (setf (slot-value transport 'host) (quri:uri-host uri))
      (setf (slot-value transport 'port) (quri:uri-port uri))
      (setf (slot-value transport 'path) (or (quri:uri-path uri) "/"))))
  transport)

(defmethod start ((entity server) (transport websocket-transport))
  (clack:clackup
   (lambda (env)
     (block nil
       ;; Return 200 OK for non-WebSocket requests
       (unless (wsd:websocket-p env) (return '(200 () ("ok"))))

       (let* ((io (wsd:make-server env))
              (connection (make-instance 'connection
                                         :io io
                                         :transport transport
                                         :entity entity
                                         )))

         (on :message io
             (lambda (input)
               ;; ------------------------------
               ;; read and (enqueue request to inbox) or (dispatch response)
               (let ((payload (handler-case
                                  ;; this part is receive-payload
                                  (message-json-to-payload input :need-jsonrpc-field-p (slot-value entity 'need-jsonrpc-field-p))
                                (jsonrpc-error ()
                                  ;; Nothing can be done
                                  nil))))
                 (when payload (connection-handle-payload connection payload)))))

         (on :open io
             (lambda ()
               (connection-prepare-destruction-hook connection)
               ;; hook on open connection
               (emit :open entity connection)
               ))

         (on :close io
             (lambda (&key code reason)
               (declare (ignore code reason))
               (emit :close connection)))

         (lambda (responder)
           (declare (ignore responder))
           (let ((thread (bt:make-thread
                          ;; ------------------------------
                          ;; handle inbox || handle outbox
                          (lambda () (connection-process-loop connection :payload-writer #'payload-writer-websocket))
                          :name "jsonrpc/transport/websocket processing")))
             (unwind-protect
                  (wsd:start-connection io)
               (bt:destroy-thread thread)))))))
   :host (slot-value transport 'host)
   :port (slot-value transport 'port)
   :server :hunchentoot
   :debug (slot-value transport 'debug)
   :use-thread nil))

(defmethod start ((entity client) (transport websocket-transport))
  (let* ((io (wsd:make-client (format nil "~A://~A:~A~A"
                                      (if (slot-value transport 'securep) "wss" "ws")
                                      (slot-value transport 'host)
                                      (slot-value transport 'port)
                                      (slot-value transport 'path))))
         (connection (make-instance 'connection :io io :transport transport :entity entity)))

    (on :open io
        (lambda ()
          (emit :open transport connection)))
    (on :close io
        (lambda (&key code reason)
          (declare (ignore code reason))
          ;; Reconnect automatically
          (wsd:start-connection io)))
    (on :message io
        ;; ------------------------------
        ;; read and (enqueue request to inbox) or (dispatch response)
        (lambda (message-json)
          (let ((payload (message-json-to-payload message-json :need-jsonrpc-field-p (slot-value entity 'need-jsonrpc-field-p))))
            (when payload
              (connection-handle-payload connection payload)))))

    (wsd:start-connection io)
    ;;(setf (slot-value transport 'connection) connection)

    (setf (slot-value entity 'threads)
          (list
           ;; ------------------------------
           ;; handle inbox || handle outbox
           (bt:make-thread (lambda () (connection-process-loop connection :payload-writer #'payload-writer-websocket))
                           :name "jsonrpc/transport/websocket processing")
           ;; KLUDGE: Requires to kill the read-thread of WebSocket client
           ;;   for calling 'close-connection'.
           ;;   Perhaps, finalization should be done in other places.
           (slot-value io 'websocket-driver.ws.client::read-thread)))
    connection))

(defun payload-writer-websocket (connection payload)
  (let ((json (jsown:to-json payload))
        (ws (slot-value connection 'io)))
    (wsd:send ws json)))
