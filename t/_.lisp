(in-package #:cl-user)
(defpackage #:jsonrpc/t
  (:use #:cl
        #:rove
        #:jsonrpc)
  (:shadowing-import-from #:rove #:*debug-on-error*))
(in-package #:jsonrpc/t)

(deftest tcp-transport
  (testing "basic"
    (let ((port (jsonrpc::random-port)))
      (let (transport)
        (setq transport (make-instance 'jsonrpc:websocket-server :url (format nil "ws://localhost:~d/a" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          ;;(format t ">>>>>>server. args:~a~%" args)
                                          (reduce #'+ args)
                                          ))
        
        (jsonrpc:start transport))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:websocket-client :url (format nil "ws://127.0.0.1:~d/a" port)))
        (setq connection (jsonrpc:start transport))
        (ok (= 60 (jsonrpc:call connection "sum" '(20 40))))))))




(deftest websocket-transport
  (testing "basic"
    (let ((port (jsonrpc::random-port)))
      ;;(bt:make-thread
      ;; (lambda (&aux server transport)a
      (let (transport)
        (setq transport (make-instance 'jsonrpc:tcp-server :url (format nil "http://127.0.0.1:~d" port)))
        (jsonrpc:expose transport "sum" (lambda (args)
                                          (format t ">>>>>>server. args:~a~%" args)
                                          (reduce #'+ args)
                                          ))
        (jsonrpc:start transport))
      
      (sleep 0.2)
      
      (let (transport connection)
        (setq transport (make-instance 'jsonrpc:tcp-client :url (format nil "http://127.0.0.1:~d" port)))
        (setq connection (jsonrpc:start transport))
        (jsonrpc:call connection "sum" '(20 40))))))
