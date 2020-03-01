(in-package #:jsonrpc)

(defvar *connection*)

(defclass connection (event-emitter)
  ((io :initarg :io)

   (transport :initarg :transport)

   ;;(threads :initform '())
   (processor :initform nil)
   (reader :initform nil)

   (inbox :initform (make-instance 'chanl:unbounded-channel))
   (outbox :initform (make-instance 'chanl:unbounded-channel))

   (response-lock :initform (bt:make-recursive-lock "jsonrpc/connection response-lock"))
   (response-map :initform (make-hash-table :test 'equal))
   (response-callback :initform (make-hash-table :test 'equal))

   ;; process-wait
   (condvar :initform (bt:make-condition-variable))
   (condlock :initform (bt:make-recursive-lock))
   ))

(defun connection-destroy (connection)
  (with-slots (thread) connection
    (when thread (bt:destroy-thread thread))))

;;;; call api

(defun call-async (connection method params &key callback &aux id)
  "You can notify with callback being nil."
  (check-type params jsonrpc-params)
  (check-type connection connection)

  (when callback
    (setq id (make-id :id-type (slot-value (slot-value connection 'transport) 'id-type)))
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

(defun connection-enoutbox-payload (connection payload)
  (when payload
    (chanl:send (slot-value connection 'outbox) payload)
    (connection-notify-ready connection)))

(defun connection-handle-request (connection request
                                  &aux (transport (slot-value connection 'transport)))
  (flet ((proc (request)
           (let ((*connection* connection)
                 (bt:*default-special-bindings* (append `((*connection* . ,connection))
                                                        bt:*default-special-bindings*)))
             (transport-request-to-response transport request))))
    (if (listp request)
        (mapcar #'proc request)
      (proc request))))

(defun connection-handle-response (connection response)
  (let ((id (response-id response)))
    (if (null id)
        ;; ----------
        ;; no-id
        (progn (warn "Unexpected response which has no id. Ignored.")
               (logd "Maybe receive notification. response:~a" response))

      (with-slots (response-map response-lock response-callback) connection
        (bt:with-recursive-lock-held (response-lock)
          (let ((callback (gethash id response-callback)))
            (if callback
                (progn
                  (handler-case
                      (funcall callback response)
                    (error (e) (vom:error "~A in a JSON-RPC response callback: ~A" (type-of e) e)))
                  (remhash id response-callback))
              (setf (gethseash id response-map) response))))))))

(defun connection-reader (connection &key payload-reader (name "reader"))
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (loop for payload = (funcall payload-reader connection)
             while payload
             do
               (chanl:send (slot-value connection 'inbox) payload)
               (connection-notify-ready connection))
       (when (bt:thread-alive-p (slot-value connection 'processor))
         (bt:destroy-thread processor))
       (setf (slot-value connection 'processor) nil
             (slot-value connection 'reader) nil)
       ))
   :name name
   ))

(defun connection-processor (connection &key payload-writer (name "processor"))
  (bt:make-thread
   (lambda ()
     (with-slots (inbox outbox transport) connection
       (unwind-protect
            (loop
               (when (and (chanl:recv-blocks-p inbox)
                          (chanl:recv-blocks-p outbox))
                 (connection-wait-for-ready connection))

               (chanl:select
                ;; ------------------------------
                ;; handle inbox
                ((chanl:recv inbox payload)
                 (cond
                   ;; request
                   ((typep payload 'request)
                    (connection-enoutbox-payload
                     connection
                     (connection-handle-request connection payload)))
                   ;; response
                   (t
                    (connection-handle-response connection payload))))

                ;; ------------------------------
                ;; handle outbox
                ((chanl:recv outbox payload)
                 (funcall payload-writer connection payload))
                ))
         (connection-close connection))))
   :name name
   ))

(defun connection-close (connection)
  (with-slots (transport) connection
    (transport-close-connection transport connection)))


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

;;(defun connection-prepare-destruction-hook (connection &aux (server (slot-value connection 'transport)))
;;  (with-slots (connections-lock connections) server
;;    (on :close connection
;;        (lambda ()
;;          (with-lock-held (connections-lock)
;;            (setf connections (delete connection connections)))))
;;    (with-lock-held (connections-lock)
;;      (push connection connections))))
