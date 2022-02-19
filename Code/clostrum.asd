(cl:in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :components
  ((:file "packages")
   (:file "clostrum")
   (:file "conditions"))
  :in-order-to ((test-op (test-op "clostrum/test"))))

(defsystem "clostrum/virtual"
  :description "Example implementation of the Clostrum protocol."
  :depends-on ("clostrum" "alexandria")
  :components
  ((:file "virtual"))
  :in-order-to ((test-op (load-op "clostrum/test")))
  :perform (test-op (operation component)
             (flet ((s (name) (uiop:find-symbol* name '#:clostrum/virtual)))
               (uiop:symbol-call
                '#:clostrum/test '#:run-tests
                (s '#:virtual-client)
                (s '#:virtual-run-time-environment)
                nil
                (s '#:virtual-compilation-environment)))))
