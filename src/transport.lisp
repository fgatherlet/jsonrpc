(in-package #:jsonrpc)

(defvar *default-timeout* 5)

(defun %transport-id-type-p (x)
  (member x '(:string :number)))
(deftype transport-id-type ()
  '(satisfies %transport-id-type-p))

(defclass transport ()
  ((exposed :initform (make-hash-table :test 'equal))

   (need-jsonrpc-field-p :initarg :need-jsonrpc-field-p :initform t)
   (id-type :initarg :id-type :initform :number :type (satisfies transport-id-type-p))
   ))

(defgeneric transport-disconnect (transport connection))
(defgeneric transport-finalize-connection (transport connection))
(defgeneric transport-alive-connection-p (transport connection))

(defun expose (transport method-name function)
  (setf (gethash method-name (slot-value transport 'exposed)) function))

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

;;;; client

(defclass client (transport) ())

(defgeneric transport-connect (client))

;;;; server

(defclass server (transport event-emitter)
  ((listener
    :initform nil
    :documentation "Server transport have listner thread. Client transport does not have thread usually.")))

(defgeneric transport-listen (server))

