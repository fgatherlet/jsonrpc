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

(defclass websocket-client (websocket-transport client) ())
(defclass websocket-server (websocket-transport server) ())

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

(defmethod transport-finalize-connection ((transport websocket-transport) (connection connection))
  (with-slots (reader processor io) connection
    (when (bt:thread-alive-p processor)
      (bt:destroy-thread processor))
    (setq reader nil
          processor nil)
    ;;(when (member (wsd:ready-state io) '(:open :opening))
    ;;(wsd:close-connection io))
    ))


(defmethod transport-connect ((transport websocket-client))
  (let* ((io (wsd:make-client (format nil "~A://~A:~A~A"
                                      (if (slot-value transport 'securep) "wss" "ws")
                                      (slot-value transport 'host)
                                      (slot-value transport 'port)
                                      (slot-value transport 'path))))
         (connection (make-instance 'connection :io io :transport transport)))

    (on :close io
        (lambda (&key code reason)
          (declare (ignore code reason))
          (with-slots (processor) connection
            (close-connection connection))))
    ;;;; finalizer
    ;;(when (bt:thread-alive-p processor) (bt:destroy-thread processor))
    ;;(setf (slot-value connection 'processor) nil))))

    (on :message io
        ;; ------------------------------
        ;; read and (enqueue request to inbox) or (dispatch response)
        (lambda (message-json)
          (let ((payload (message-json-to-payload message-json :need-jsonrpc-field-p (slot-value transport 'need-jsonrpc-field-p))))
            (when payload
              (chanl:send (slot-value connection 'inbox) payload)
              (connection-notify-ready connection)
              ))))

    (setf (slot-value connection 'processor)
          (connection-processor connection
                                :name "jsownrpc/websocket-client/processor"
                                :payload-writer #'%payload-writer-websocket))

    ;; main (reader) thread. guess synchronous.
    (wsd:start-connection io)

    connection))

(defmethod transport-listen ((transport websocket-server))
  (with-slots (listener) transport
    (when listener (error 'transport-already-listening))

    (setf
     listener
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
                    )))

            (on :close io
                (lambda (&key code reason)
                  (declare (ignore code reason))
                  (transport-finalize-connection (slot-value connection 'transport) connection)
                  ))

            (lambda (responder)
              (declare (ignore responder))

              (with-slots (processor reader) connection
                (setf processor (connection-processor connection :name "jsownrpc/websocket-server/processor" :payload-writer #'%payload-writer-websocket))
                (setf reader (bt:current-thread))
                (unwind-protect
                     ;; main (reader) thread
                     (wsd:start-connection io)

                  ;; finalizer
                  (wsd:close-connection io)
                  (when (bt:thread-alive-p processor) (bt:destroy-thread processor))
                  (setf processor nil reader nil)
                  ))))))

      :host (slot-value transport 'host)
      :port (slot-value transport 'port)
      :server :hunchentoot
      :debug (slot-value transport 'debug)
      :use-thread t))))


(defun %payload-writer-websocket (connection payload)
  (let ((json (jsown:to-json payload))
        (ws (slot-value connection 'io)))
    (wsd:send ws json)))
