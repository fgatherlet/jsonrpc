(in-package #:jsonrpc)

(defvar *default-timeout* 60)

;;;; entity

(defclass entity (event-emitter)
  ((need-jsonrpc-field-p :initarg :need-jsonrpc-field-p :initform t)
   (exposed :initform (make-hash-table :test 'equal))

   (id-type :initarg :id-type :initform :string)
   ))


(defun expose (entity method-name function)
  (setf (gethash method-name (slot-value entity 'exposed)) function))

(defun clear-exposed (entity)
  (setf (slot-value object 'exposed) (make-hash-table :test 'equal))
  (values))

(defun entity-request-to-response (entity request
                                &aux
                                  (exposed (slot-value entity 'exposed))

                                  (method (request-method request))
                                  (params (request-params request))
                                  (id (request-id request))

                                  (fn (gethash method exposed))

                                  result)

  (handler-bind
      ((jsonrpc-error
        (lambda (e)
          (return-from entity-request-to-response
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

          (return-from entity-request-to-response
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

(defclass server (entity)
  ((connections :initform '())
   (%lock :initform (bt:make-lock "connections-lock"))))

;;;;

(defclass client (entity) ())

(defun client-disconnect (client)
  (with-slots (thread) client
    (bt:destroy-thread thread)
    (setf thread nil))
  (emit :close client)
  (values))
