(cl:in-package #:asdf-user)

;;; This system contains an example implementation of the clostrum
;;; protocol using hash tables. It is not tied to any implementation
;;; internals.

;;; TODO
;;;
;;; - specify error conditions in documentation and use them

(defsystem "clostrum-basic"
  :description "Example implementation of the Clostrum protocol."
  :depends-on ("clostrum")
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "entry")
   (:file "basic")
   (:file "run-time"))
  :in-order-to ((test-op (load-op "clostrum-test")))
  :perform (test-op (operation component)
             (flet ((s (name) (uiop:find-symbol* name '#:clostrum-basic)))
               (uiop:symbol-call
                '#:clostrum/test '#:run-tests
                nil
                (s '#:run-time-environment)
                nil
                (s '#:compilation-environment)))))
