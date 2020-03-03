(in-package #:jsonrpc)

(deftype jsonrpc-params () '(or list array hash-table))

(defun port-available-p (port)
  (handler-case (let ((socket (usocket:socket-listen "127.0.0.1" port :reuse-address t)))
                  (usocket:socket-close socket))
    (usocket:address-in-use-error (e) (declare (ignore e)) nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
     if (port-available-p port)
     return port))

(defvar *id* 0)
(defun make-id (&key (length 12) (id-type :string))
  (declare (type fixnum length))

  (when (eql :number id-type) (return-from make-id (incf *id*)))

  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))

(defun find-transport-class (mode)
  (find-class (intern (format nil "~a-TRANSPORT" mode) :jsonrpc)))

(defun val (jsown key)
  (jsown:val-safe jsown key))

(defun (setf val) (val jsown key)
  (setf (jsown:val jsown key) val))

(defun remkey (jsown key)
  (jsown:remkey jsown key))

(defun keys (jsown)
  (mapcar #'car
          (cdr jsown)))

(defmacro funcall-handler-binded (fn params)
  (let ((e (gensym "E-"))
        (message (gensym "message-")))
    `(handler-bind (#+ccl
                    (ccl::wrong-number-of-arguments
                     (lambda (,e)
                       (declare (ignore ,e))
                       (error 'jsonrpc-invalid-params)))
                    #+sbcl
                    (sb-int:simple-program-error
                     (lambda (,e)
                       (let ((,message (simple-condition-format-control ,e)))
                         (when (equal ,message "invalid number of arguments: ~S")
                           (error 'jsonrpc-invalid-params))))))
       (funcall ,fn ,params))))

(defconstant +loglevel-debug+ 1)
(defconstant +loglevel-info+ 2)
(defconstant +loglevel-warn+ 3)
(defconstant +loglevel-error+ 3)
(defvar *loglevel* +loglevel-error+)
(defun logd (&rest rest)
  (when (<= *loglevel* +loglevel-debug+)
    (format t "~&D ")
    (apply #'format t rest)
    (write-char #\newline)))
