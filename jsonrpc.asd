(defsystem "jsonrpc"
  ;;:class :package-inferred-system
  :version "0.3.2"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "JSON-RPC 2.0 server/client implementation"
  ;;:depends-on ("jsonrpc/main")
  :depends-on (:bordeaux-threads
               :trivial-utf-8
               :usocket
               :fast-io
               :vom
               :chanl
               :dissect
               :event-emitter
               :quri
               :websocket-driver
               :clack
               :jsown)
  :components ((:module
                "src"
                :components
                ((:file "_")
                 (:module
                  "base"
                  :components
                  ((:file "utils")
                   (:file "errors")
                   ;;(:file "mapper")
                   ))
                 (:file "payload")
                 (:file "connection")

                 ;;(:file "entity")

                 (:file "transport" :depends-on (:connection))
                 (:file "transport-tcp" :depends-on (:transport))
                 (:file "transport-websocket" :depends-on (:transport))
                 ;;(:file "transport-stdio")
                 ;;(:module
                 ;; "transport"
                 ;; :depends-on (:connection)
                 ;; :components
                 ;; ((:file "_")
                 ;;  (:file "stdio")
                 ;;  (:file "tcp")
                 ;;  (:file "websocket")))

                 ;;(:module
                 ;; "entity"
                 ;; :depends-on (:transport)
                 ;; :components
                 ;; ((:file "_")
                 ;;  (:file "client")
                 ;;  (:file "server")))
                 )))
  :in-order-to ((test-op (test-op "jsonrpc/t")))
  )


(asdf:register-system-packages "clack-handler-hunchentoot" '(#:clack.handler.hunchentoot))

(defsystem "jsonrpc/t"
  :class :package-inferred-system
  :depends-on ("rove"
               "jsonrpc/tests/request-response"
               "jsonrpc/tests/transport/tcp"
               "jsonrpc/tests/transport/stdio"
               "jsonrpc/tests/transport/websocket")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
