(in-package #:jsonrpc)

(defclass websocket-transport ()
  ((url :initarg :url :initform nil)
   (debug :initarg :debug
          :initform t)))

(defmethod initialize-instance :after ((transport websocket-transport) &rest initargs &key url &allow-other-keys)
  (declare (ignore initargs))
  (when url
    (let ((uri (quri:uri url)))
      (unless (member (quri:uri-scheme uri) '("ws" "wss") :test #'equalp)
        (error "Only ws or wss are supported for websocket-transport (specified ~S)" (quri:uri-scheme uri)))

      (setf (slot-value transport 'url) url)
      ))
  transport)

(defmethod transport-disconnect ((transport websocket-transport) (connection connection))
  (wsd:close-connection (slot-value connection 'io)))

(defmethod transport-finalize-connection ((transport websocket-transport) (connection connection))
  (with-slots (processor io) connection
    (when (bt:thread-alive-p processor) (bt:destroy-thread processor))
    (setq processor nil)
    ))

(defmethod transport-alive-connection-p ((transport websocket-transport) connectionh)
  (and (slot-value connectionh 'io)
       (eql (wsd:ready-state (slot-value connectionh 'io)) :open)))

(defun %payload-writer-websocket (connection payload)
  (let ((json (jsown:to-json payload))
        (ws (slot-value connection 'io)))
    (wsd:send ws json)))

;;;; client

(defclass websocket-client (websocket-transport client) ())

(defmethod transport-connect ((transport websocket-client))
  (let* ((io (wsd:make-client (slot-value transport 'url)))
         (connection (make-instance 'connection :io io :transport transport)))

    (on :close io
        (lambda (&key code reason)
          (declare (ignore code reason))
          (transport-finalize-connection transport connection)))

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

;;;; server

(defclass websocket-server (websocket-transport server) ())

(defmethod transport-listen ((transport websocket-server)
                             &aux
                               (url (slot-value transport 'url))
                               (uri (quri:uri url))
                               )
  (let (app)
    (unwind-protect
         (setq app (clack:clackup
                    (lambda (env)
                      (block nil
                        ;; Return 200 OK for non-WebSocket requests
                        (unless (wsd:websocket-p env) (return '(200 () ("ok"))))

                        ;;(logd "env:~a (~s)" env env)
                        (unless (equal (quri:uri-path uri) (getf env :path-info))
                          (return '(404 () ("unknown endpoint"))))

                        ;; new connection
                        (let* ((io (wsd:make-server env))
                               (connection (make-instance 'connection :io io :transport transport)))

                          (on :close io
                              (lambda (&key code reason)
                                (declare (ignore code reason))
                                (transport-finalize-connection (slot-value connection 'transport) connection)
                                ))

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

                          (lambda (responder)
                            (declare (ignore responder))

                            (setf (slot-value connection 'processor)
                                  (connection-processor connection
                                                        :name "jsownrpc/websocket-server/processor"
                                                        :payload-writer #'%payload-writer-websocket))

              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            ;; main (reader) thread. guess synchronous.
                            (unwind-protect
                                 (wsd:start-connection io)
                              (wsd:close-connection io))
                            ))))
                    :host (quri:uri-host uri)
                    :port (quri:uri-port uri)
                    :server :hunchentoot
                    :debug (slot-value transport 'debug)
                    :use-thread nil))
      ;; fail to laucnh or thread break
      (progn
        (logd "clack:stop app from")
        (ignore-errors (when app (clack:stop app)))
        (logd "clack:stop app to")
        ))))

;;(defmethod transport-dispose-listener ((transport websocket-server))
;;  (with-slots (listener) transport
;;    (when (and listener
;;               (not (eql :disposed listener)))
;;      (logd "dispose listener(~a)" listener)
;;      (clack:stop listener))
;;    (setf listener :disposed)))
;;           ;; KLUDGE: Requires to kill the read-thread of WebSocket client
;;           ;;   for calling 'close-connection'.
;;           ;;   Perhaps, finalization should be done in other places.
;;           (slot-value io 'websocket-driver.ws.client::read-thread)))
