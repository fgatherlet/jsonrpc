(in-package #:jsonrpc)

(defclass websocket-transport ()
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

(defclass websocket-server (websocket-transport server) ())
(defclass websocket-client (websocket-transport client) ())

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

(defmethod transport-close-connection ((transport websocket-transport) (connection connection))
  (with-slots (io) connection
    ;;(finish-output io)
    (wsd:close-connection io))
  (call-next-method))


(defmethod start ((transport websocket-server))
  (clack:clackup
   (lambda (env)
     (block nil
       ;; Return 200 OK for non-WebSocket requests
       (unless (wsd:websocket-p env) (return '(200 () ("ok"))))

       (let* ((io (wsd:make-server env))
              (connection (make-instance 'connection
                                         :io io
                                         :transport transport
                                         )))

         (on :message io
             (lambda (input)
               ;; ------------------------------
               ;; read and (enqueue request to inbox) or (dispatch response)
               (let ((payload (handler-case
                                  (message-json-to-payload input :need-jsonrpc-field-p (slot-value transport 'need-jsonrpc-field-p))
                                (jsonrpc-error ()
                                  ;; Nothing can be done
                                  nil))))
                 ;; enqueue to inbox
                 (when payload
                   (chanl:send (slot-value connection 'inbox) payload)
                   (connection-notify-ready connection))
                 ;;(when payload (connection-handle-payload connection payload))
                 )))

         (on :open io
             (lambda ()
               ;; (connection-prepare-destruction-hook connection)
               ;;(let ((server (slot-value connection 'transport)))
               ;;  (with-slots (connections-lock connections) server
               ;;    (on :close connection
               ;;        (lambda ()
               ;;          (with-lock-held (connections-lock)
               ;;            (setf connections (delete connection connections)))))
               ;;    (with-lock-held (connections-lock)
               ;;      (push connection connections))))

               ;; hook on open connection
               (emit :open transport connection)
               ))

         (on :close io
             (lambda (&key code reason)
               (declare (ignore code reason))
               (emit :close connection)))

         (lambda (responder)
           (declare (ignore responder))
           (let ((processor (connection-processor connection :name "jsownrpc/websocket-server/processor"
                                                  :payload-writer #'payload-writer-websocket)))
             (unwind-protect
                  (wsd:start-connection io)
               (bt:destroy-thread processor)))))))

   :host (slot-value transport 'host)
   :port (slot-value transport 'port)
   :server :hunchentoot
   :debug (slot-value transport 'debug)
   :use-thread t))

(defmethod start ((transport websocket-client))
  (let* ((io (wsd:make-client (format nil "~A://~A:~A~A"
                                      (if (slot-value transport 'securep) "wss" "ws")
                                      (slot-value transport 'host)
                                      (slot-value transport 'port)
                                      (slot-value transport 'path))))
         (connection (make-instance 'connection :io io :transport transport)))

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
          (let ((payload (message-json-to-payload message-json :need-jsonrpc-field-p (slot-value transport 'need-jsonrpc-field-p))))
            (when payload
              ;;(connection-handle-payload connection payload)
              (chanl:send (slot-value connection 'inbox) payload)
              (connection-notify-ready connection)
              ))))

    (wsd:start-connection io)
    ;;(setf (slot-value transport 'connection) connection)

    (setf (slot-value connection 'threads)
          (list
           (connection-processor connection :name "jsownrpc/websocket-client/processor"
                                 :payload-writer #'payload-writer-websocket)
           ;; KLUDGE: Requires to kill the read-thread of WebSocket client
           ;;   for calling 'close-connection'.
           ;;   Perhaps, finalization should be done in other places.
           (slot-value io 'websocket-driver.ws.client::read-thread)))
    connection))

(defun payload-writer-websocket (connection payload)
  (let ((json (jsown:to-json payload))
        (ws (slot-value connection 'io)))
    (wsd:send ws json)))
