(cl:in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :components
  ((:file "packages")
   (:file "clostrum")
   (:file "conditions"))
  :in-order-to ((test-op (test-op "clostrum-test"))))
