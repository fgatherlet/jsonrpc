(in-package #:cl-user)
(defpackage #:jsonrpc/t
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:shadowing-import-from #:rove #:*debug-on-error*))
(in-package #:jsonrpc/t)

(defun wait-until (condition &key (timeout 10) &aux timeout-internal-time)
  (setf timeout-internal-time
        (+ (get-internal-run-time)
           (* timeout internal-time-units-per-second)))
  (loop
     (let ((res (funcall condition)))
       (when res (return-from wait-until)))
     (when (< timeout-internal-time (get-internal-run-time))
       (return-from wait-until))
     (sleep 0.1)
     ))

(defvar *ws-url* "ws://127.0.0.1:9999/a")
(defun update-ws-url ()
  (setf *ws-url* (format nil
                         "ws://127.0.0.1:~d/a"
                         (jsonrpc::random-port)
                         )))

(defvar *server*)
(defvar *listener*)

(defvar *server1*)
(defvar *listener1*)

(defvar *client*)
(defvar *connection*)

(deftest websocket
  (testing "port-bind-error-handle"
    (progn
      (setq *server* (make-instance 'jsonrpc:websocket-server :url *ws-url*))
      (setq *listener* (bt:make-thread (lambda () (jsonrpc:transport-listen *server*))))
      (sleep 0.1)
      
      ;; (jsonrpc:transport-listen *server*)
      ;; USOCKET:ADDRESS-IN-USE-ERROR
      (ok (signals (jsonrpc:transport-listen *server*) 'error))
      (sleep 0.1)
      
      (bt:destroy-thread *listener*)
      (sleep 0.1)
      
      (setq *listener* (bt:make-thread (lambda () (jsonrpc:transport-listen *server*))))
      (sleep 0.1)
      (bt:destroy-thread *listener*)))

  (sleep 0.5)
  
  (testing "connection-close-check"
    (setq *server* (make-instance 'jsonrpc:websocket-server :url *ws-url*))
    (jsonrpc:expose *server* "sum" (lambda (arg)
                                     (reduce #'+ arg)))
    (setq *listener* (bt:make-thread (lambda () (jsonrpc:transport-listen *server*))))
    (sleep 0.1)

    (setq *client* (make-instance 'jsonrpc:websocket-client :url *ws-url*))
    (setq *connection* (jsonrpc:transport-connect *client*))
    (sleep 0.1)

    (ok (jsonrpc:alivep *connection*))
    (ok (= 6 (jsonrpc:call *connection* "sum" '(1 2 3))))

    (jsonrpc:disconnect *connection*)
    (sleep 0.1)

    (ok (null (jsonrpc:alivep *connection*)))
    (ok (signals (jsonrpc:call *connection* "sum" '(1 2 3)) 'error))
    (sleep 0.1)
    ))


(deftest etc
  (testing "basic"

    (let ((port (jsonrpc::random-port)))
      (let (transport)
        (setq transport (make-instance 'jsonrpc:tcp-server :url (format nil "http://127.0.0.1:~d" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          (reduce #'+ args)
                                          ))
        (bt:make-thread (lambda () (jsonrpc:transport-listen transport))))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:tcp-client :url (format nil "http://127.0.0.1:~d" port)))
        (setq connection (jsonrpc:transport-connect transport))
        (ok (= 60 (jsonrpc:call connection "sum" '(20 40))))
        (jsonrpc::disconnect connection)
        
        (sleep 0.2)
        ;;(wait-until (lambda () (not (jsonrpc::alivep connection))))
        
        (ok (signals (jsonrpc:call connection "sum" '(20 40))
                     'jsonrpc::connection-is-dead))
        ))

    


    (let ((port (jsonrpc::random-port)))
      (let (transport)
        (setq transport (make-instance 'jsonrpc:websocket-server :url (format nil "ws://localhost:~d/a" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          ;;(format t ">>>>>>server. args:~a~%" args)
                                          (reduce #'+ args)
                                          ))
        (bt:make-thread (lambda () (jsonrpc:transport-listen transport))))
      
      (sleep 0.2)
      
      (let (transport connection connection1)
        (setq transport (make-instance 'jsonrpc:websocket-client :url (format nil "ws://127.0.0.1:~d/a" port)))
        (setq connection (jsonrpc:transport-connect transport))
        (ok (= 60 (jsonrpc:call connection "sum" '(20 40))))
        
        (setq connection1 (jsonrpc:transport-connect transport))
        (ok (= 3 (jsonrpc:call connection "sum" '(1 2))))
        ))
    
    (let ((port (jsonrpc::random-port)))
      (let (transport)
        (setq transport (make-instance 'jsonrpc:websocket-server :url (format nil "ws://localhost:~d/a" port)))
        (jsonrpc:expose transport "sum" (lambda (args) (reduce #'+ args)))
        
        (bt:make-thread (lambda () (jsonrpc:transport-listen transport))))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:websocket-client :url (format nil "ws://127.0.0.1:~d/b" port)))
        (ok (signals
             (jsonrpc:transport-connect transport)
             ;; TODO: bizarre condition on 404 error.
             'FAST-HTTP.ERROR:CB-FIRST-LINE)))
      )

    
    ))

(deftest tcp-transport
  (testing "basic"
    (let ((port (jsonrpc::random-port)))
      (let (transport)
        (setq transport (make-instance 'jsonrpc:websocket-server :url (format nil "ws://localhost:~d/a" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          ;;(format t ">>>>>>server. args:~a~%" args)
                                          (reduce #'+ args)
                                          ))
        
        (bt:make-thread (lambda () (jsonrpc:transport-listen transport))))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:websocket-client :url (format nil "ws://127.0.0.1:~d/a" port)))
        (setq connection (jsonrpc:transport-connect transport))
        (ok (= 60 (jsonrpc:call connection "sum" '(20 40))))))))




(deftest websocket-transport
  (testing "basic"
    (let ((port (jsonrpc::random-port)))
      ;;(bt:make-thread
      ;; (lambda (&aux server transport)
      (let (transport)
        (setq transport (make-instance 'jsonrpc:tcp-server :url (format nil "http://127.0.0.1:~d" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          (format t ">>>>>>server. args:~a~%" args)
                                          (reduce #'+ args)
                                          ))
        (bt:make-thread (lambda () (jsonrpc:transport-listen transport))))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:tcp-client :url (format nil "http://127.0.0.1:~d" port)))
        (setq connection (jsonrpc:transport-connect transport))
        (ok (= 60 (jsonrpc:call connection "sum" '(20 40))))))))



