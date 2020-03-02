(in-package #:jsonrpc)

(defvar *debug-on-error* nil
  "Open an interactive debugger on any error.")

(define-condition jsonrpc-error (error)
  ((code :initarg :code
         :initform -1)
   (message :initarg :message
            :initform "")))

(define-condition jsonrpc-parse-error (jsonrpc-error)
  ((code :initform -32700)
   (message :initform "Parse error")))

(define-condition jsonrpc-invalid-request (jsonrpc-error)
  ((code :initform -32600)
   (message :initform "Invalid Request")))

(define-condition jsonrpc-invalid-response (jsonrpc-error)
  ((code :initform -32000)
   (message :initform "Invalid Response")))

(define-condition jsonrpc-method-not-found (jsonrpc-error)
  ((code :initform -32601)
   (message :initform "Method not found")))

(define-condition jsonrpc-invalid-params (jsonrpc-error)
  ((code :initform -32602)
   (message :initform "Invalid params")))

(define-condition jsonrpc-internal-error (jsonrpc-error)
  ((code :initform -32603)
   (message :initform "Internal error")))

(define-condition jsonrpc-server-error (jsonrpc-error) ())

(define-condition jsonrpc-callback-error (jsonrpc-error) ()
  (:report (lambda (condition stream)
             (with-slots (message code) condition
               (format stream "JSONRPC-CALLBACK-ERROR: ~A (Code=~A)" message code)))))

(defmethod jsown:to-json ((obj jsonrpc-error))
  (jsown:to-json
   `(:obj ("code" . ,(slot-value obj 'code))
          ("message" . ,(slot-value obj 'message)))))


(define-condition transport-already-listening (error) ())
(define-condition connection-is-dead (error) ())
