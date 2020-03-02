(in-package #:cl-user)
(defpackage #:jsonrpc
  (:use #:cl)
  ;;; class
  (:import-from #:event-emitter
                #:on
                #:emit
                #:event-emitter)
  (:import-from #:alexandria
                #:hash-table-keys
                #:xor
                #:remove-from-plist)
  (:import-from #:usocket
                #:socket-listen
                #:socket-close
                #:address-in-use-error)
  (:import-from #:bordeaux-threads
                #:make-condition-variable
                #:make-recursive-lock
                #:with-recursive-lock-held
                #:condition-wait
                #:condition-notify
                #:*default-special-bindings*
                #:destroy-thread
                #:make-lock
                #:with-lock-held)
  (:import-from #:dissect
                #:present)
  (:import-from #:chanl)
  (:import-from #:vom)
  ;;(:import-from #:zason
  ;;              #:with-output
  ;;              #:parse
  ;;              #:encode
  ;;              #:with-object
  ;;              #:encode-object-element)
  (:import-from #:fast-io
                #:make-output-buffer
                #:finish-output-buffer
                #:fast-write-byte)
  (:import-from #:trivial-utf-8
                #:utf-8-bytes-to-string
                #:string-to-utf-8-bytes)

  (:export

   ;; from errors
   #:jsonrpc-error
   #:jsonrpc-parse-error
   #:jsonrpc-invalid-request
   #:jsonrpc-invalid-response
   #:jsonrpc-method-not-found
   #:jsonrpc-invalid-params
   #:jsonrpc-internal-error
   #:jsonrpc-server-error
   #:jsonrpc-callback-error

   #:*debug-on-error*

   ;;; base
   #:expose
   #:clear-exposed

   #:connection

   ;;; entity
   ;;#:entity

   #:client
   ;;#:client-disconnect
   #:server

   #:call-async
   #:call

   ;;; payload
   #:request
   #:response

   ;;; transport
   #:transport
   #:tcp-transport
   #:tcp-server
   #:tcp-client
   #:websocket-transport
   #:websocket-server
   #:websocket-client
   ;;#:stdio-transport cannot understand the neccesity.

   #:transport-listen
   #:transport-connect
   ))
