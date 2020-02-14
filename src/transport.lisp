(in-package #:jsonrpc)
(defclass transport (event-emitter) ())

(defgeneric start (entity transport))




