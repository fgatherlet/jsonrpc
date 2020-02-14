(in-package #:jsonrpc)

(defun message-json-to-payload (message-json &key (need-jsonrpc-field-p t) &aux message-js)
  (check-type message-json string)
  (flet ((message-js-to-payload (message-js &key (need-jsonrpc-field-p t))
           ;; message-js is jsown object.
           (if (val message-js "method")
               (progn
                 (unless (valid-request-message-js-p message-js :need-jsonrpc-field-p need-jsonrpc-field-p)
                   (error 'jsonrpc-invalid-request))
                 (make-request :method (val message-js "method")
                               :params (val message-js "params")
                               :id (val message-js "id")))
             (progn
               (unless (valid-response-message-js-p message-js :need-jsonrpc-field-p need-jsonrpc-field-p)
                 (error 'jsonrpc-invalid-response))
               (make-response :result (val message-js "result")
                              :error (val message-js "error")
                              :id (val message-js "id"))))))

    (unless (< 0 (length message-json)) (return-from message-json-to-payload))

    (setq message-js (handler-case (jsown:parse message-json) (error () (error 'jsonrpc-parse-error))))

    (if (eql :obj (car message-js))
        (message-js-to-payload message-js :need-jsonrpc-field-p need-jsonrpc-field-p)
      (mapcar (lambda (message-js) (funcall #'message-js-to-payload message-js
                                            :need-jsonrpc-field-p need-jsonrpc-field-p))
              message-js))))

;;; request

(defstruct request
  method
  params ;; jsown
  id)

(defun valid-request-message-js-p (message-js &key (need-jsonrpc-field-p t))
  (and (if need-jsonrpc-field-p
           (equal (val message-js "jsonrpc") "2.0")
         t)
       (stringp (val message-js "method"))
       (typep (val message-js "params") '(or list))
       (typep (val message-js "id") '(or string number null))
       ;; needp?
       (every (lambda (key) (member key '("jsonrpc" "method" "params" "id") :test #'string=)) (keys message-js))))

(defmethod jsown:to-json ((message-js request))
  (jsown:to-json
   `(:obj
     ("jsonrpc" . "2.0")
     ("method" . ,(request-method message-js))
     ("params" . ,(request-params message-js))
     ,@(when (request-id message-js)
         `(("id" . ,(request-id message-js)))))))

;;; response

(defstruct response
  error
  result
  id)

(defun valid-response-message-js-p (message-js &key (need-jsonrpc-field-p t))
  (and (if need-jsonrpc-field-p
           (equal (val message-js "jsonrpc") "2.0")
         t)
       (typep (val message-js "error") '(or null list))
       (typep (val message-js "id") '(or string number null))
       (xor (nth-value 1 (val message-js "error"))
            (nth-value 1 (val message-js "result")))
       (every (lambda (key) (member key '("jsonrpc" "result" "error" "id") :test #'string=)) (keys message-js))))

(defmethod jsown:to-json ((payload response))
  (jsown:to-json
   `(:obj
     ("jsonrpc" . "2.0")
     ,@(if (response-error payload)
           `(("error" . ,(response-error payload)))
         `(("result" . ,(response-result payload))))
     ("id" . ,(response-id payload)))))

(defmacro with-response ((response result error-condition) &body body)
  (let ((error-js (gensym "ERROR-JS-")))
    `(let (,result ,error-condition)
       (if (response-error ,response)
           (let ((,error-js (response-error ,response)))
             (setq ,error-condition (make-condition 'jsonrpc-callback-error
                                                    :code (val ,error-js "code")
                                                    :message (val ,error-js "message"))))
         (setq ,result (response-result ,response)))
       ,@body)))

(defun make-error-response (&key id code message (data nil data-specified-p))
  (let ((jsown (jsown:new-js)))
    (setf (val jsown "code") code
          (val jsown "message") message)
    (when data-specified-p
      (setf (val jsown "data") data))
    (make-response :error jsown :id id)))
