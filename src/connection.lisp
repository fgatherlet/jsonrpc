(in-package #:jsonrpc)

(defvar *connection*)

(defclass connection (event-emitter)
  ((io :initarg :io)

   (entity :initarg :entity)
   (transport :initarg :transport)

   (inbox :initform (make-instance 'chanl:unbounded-channel))
   (outbox :initform (make-instance 'chanl:unbounded-channel))

   (response-lock :initform (bt:make-recursive-lock "jsonrpc/connection response-lock"))
   (response-map :initform (make-hash-table :test 'equal))
   (response-callback :initform (make-hash-table :test 'equal))

   ;; process-wait
   (condvar :initform (bt:make-condition-variable))
   (condlock :initform (bt:make-recursive-lock))
   ))

;;;; call api

(defun call-async (connection method params &key callback &aux id)
  "You can notify with callback being nil."
  (check-type params jsonrpc-params)
  (check-type connection connection)

  (when callback
    (setq id (make-id :id-type (slot-value (slot-value connection 'entity) 'id-type)))
    (connection-set-callback-for-response connection id callback))

  (connection-enoutbox-payload connection (make-request :id id :method method :params params))

  (values))

(defvar *call-result* (make-hash-table :test 'eq))

(defun call (connection method params &key (timeout *default-timeout*))
  (check-type params jsonrpc-params)
  (check-type connection connection)

  (let ((result-lock (bt:make-lock))
        (result-cond (bt:make-condition-variable)))

    (call-async connection method params
                :callback
                (lambda (response)
                  (with-lock-held (result-lock)
                    (with-response (response result error-condition)
                      (setf (gethash result-lock *call-result*) (or error-condition result))
                      (bt:condition-notify result-cond)))))

    (with-lock-held (result-lock)

      (unless (bt:condition-wait result-cond result-lock :timeout timeout) (error "JSON-RPC synchronous call has been timeout"))

      (multiple-value-bind (result result-exists-p) (gethash result-lock *call-result*)
        (assert result-exists-p)
        (remhash result-lock *call-result*)
        (when (typep result 'condition) (error result))
        result))))

;;;;


(defun connection-wait-for-ready (connection)
  (with-slots (condlock condvar) connection
    (bt:with-recursive-lock-held (condlock)
      (bt:condition-wait condvar condlock))))

(defun connection-notify-ready (connection)
  (bt:with-recursive-lock-held ((slot-value connection 'condlock))
    (bt:condition-notify (slot-value connection 'condvar))))

(defun connection-handle-payload (connection payload)
  (cond
    ((listp payload)
     (if (typep (car payload) 'request)
         (progn
           (chanl:send (slot-value connection 'inbox) payload)
           (connection-notify-ready connection))
       (dolist (response payload)
         (connection-handle-payload connection response)))
     (values))

    ;;;; request
    ((typep payload 'request)
     (chanl:send (slot-value connection 'inbox) payload)
     (connection-notify-ready connection)
     (values))

    ;;;; response
    (t
     (let ((id (response-id payload)))
       (unless id
         (warn "Unexpected response which has no id. Ignored.")
         (logd "Maybe receive notification. payload:~a" payload)
         (return-from connection-handle-payload))
       (with-slots (response-map response-lock response-callback) connection
         (bt:with-recursive-lock-held (response-lock)
           (let ((callback (gethash id response-callback)))
             (if callback
                 (progn
                   (handler-case
                       (funcall callback payload)
                     (error (e) (vom:error "~A in a JSON-RPC response callback: ~A" (type-of e) e)))
                   (remhash id response-callback))
               (setf (gethseash id response-map) payload))))))
     (values))))

(defun connection-enoutbox-payload (connection payload)
  (when payload
    (chanl:send (slot-value connection 'outbox) payload)
    (connection-notify-ready connection)))

(defun connection-request-to-response (connection request &aux (entity (slot-value connection 'entity)))
  (flet ((proc (request)
           (let ((*connection* connection)
                  (bt:*default-special-bindings* (append `((*connection* . ,connection))
                                                         bt:*default-special-bindings*)))
             (entity-request-to-response entity request))))
    (if (listp request)
        (mapcar #'proc request)
      (proc request))))


(defun connection-read-loop (connection &key payload-reader)
  (loop for payload = (funcall payload-reader connection) ;;(%receive-payload-tcp connection)
     while payload
     do (connection-handle-payload connection payload)))



(defun connection-process-loop (connection &key payload-writer)
  (with-slots (inbox outbox transport) connection
    (loop
       (when (and (chanl:recv-blocks-p inbox)
                  (chanl:recv-blocks-p outbox))
         (connection-wait-for-ready connection))

       (chanl:select

         ;;;; handle inbox
         ((chanl:recv inbox request)
          (connection-enoutbox-payload connection (connection-request-to-response connection request)))

         ;;;; handle outbox
         ((chanl:recv outbox payload)
          (funcall payload-writer connection payload))
         ;;(transport-send-payload transport connection payload))
         ))))

;;;;

(defun connection-set-callback-for-response (connection id callback)
  (with-slots (response-map response-callback response-lock) connection
    (bt:with-recursive-lock-held (response-lock)
      (multiple-value-bind (response existsp) (gethash id response-map)
        (if existsp
            (progn
              (funcall callback response)
              (remhash id response-map))
          (setf (gethash id response-callback) callback))))
    (values)))

(defun connection-prepare-destruction-hook (connection &aux (server (slot-value connection 'entity)))
  (with-slots (%lock connections) server
    (on :close connection
        (lambda ()
          (with-lock-held (%lock)
            (setf connections (delete connection connections)))))
    (with-lock-held (%lock)
      (push connection connections))))



