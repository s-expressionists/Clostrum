(in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :components
  ((:file "packages")
   (:file "clostrum"))
  :in-order-to ((test-op (test-op "clostrum/test"))))

(defsystem "clostrum/virtual"
  :description "Example implementation of the Clostrum protocol."
  :depends-on ("clostrum")
  :in-order-to ((test-op (test-op "clostrum/test"))))

(defsystem "clostrum/test"
  :description "Test suite for a Clostrum implementation."
  :depends-on ("clostrum" "fiveam"))
