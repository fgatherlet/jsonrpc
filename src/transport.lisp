(in-package #:jsonrpc)

(defvar *default-timeout* 60)

(defun %transport-id-type-p (x)
  (member x '(:string :number)))
(deftype transport-id-type ()
  '(satisfies %transport-id-type-p))

(defclass transport (event-emitter)
  ((thread
    :initform nil
    :documentation "Server transport have listner thread. Client transport does not have thread usually.")

   (connections :initform '())

   (exposed :initform (make-hash-table :test 'equal))
   (need-jsonrpc-field-p :initarg :need-jsonrpc-field-p :initform t)
   (id-type :initarg :id-type :initform :number :type (satisfies transport-id-type-p))
   ))

(defgeneric start (transport))

(defun expose (transport method-name function)
  (setf (gethash method-name (slot-value transport 'exposed)) function))

(defun clear-exposed (transport)
  (setf (slot-value transport 'exposed) (make-hash-table :test 'equal))
  (values))

(defun transport-request-to-response (transport request
                                &aux
                                  (exposed (slot-value transport 'exposed))

                                  (method (request-method request))
                                  (params (request-params request))
                                  (id (request-id request))

                                  (fn (gethash method exposed))

                                  result)

  (handler-bind
      ((jsonrpc-error
        (lambda (e)
          (return-from transport-request-to-response
            (when id
              (make-error-response
               :id id
               :code (slot-value e 'code)
               :message (slot-value e 'message))))))
       (error
        (lambda (e &aux (e1 (make-condition 'jsonrpc-internal-error)))

          (if *debug-on-error*
              (invoke-debugger e)
            (dissect:present e))

          (return-from transport-request-to-response
            (when id
              (make-error-response
               :id id
               :code (slot-value e1 'code)
               :message (slot-value e1 'message)))))))
    ;; main
    (unless fn (error 'jsonrpc-method-not-found))

    (setq result (funcall-handler-binded fn params))

    (when id (make-response :id id :result result))))

;;;;

(defclass server (transport)
  ((connections :initform '())
   (%lock :initform (bt:make-lock "connections-lock"))))

;;;;

(defclass client (transport) ())

(defun client-disconnect (client)
  (with-slots (thread) client
    (bt:destroy-thread thread)
    (setf thread nil))
  (emit :close client)
  (values))
