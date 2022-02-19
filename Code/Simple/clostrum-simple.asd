(cl:in-package #:asdf-user)

;;; This system contains an example naive implementation of the
;;; clostrum protocol using hash tables.

;;; TODO
;;;
;;; - add entries like function-inline in the compilation environment
;;; - specify error conditions in documentation and use them

(defsystem "clostrum-simple"
  :description "Example naive implementation of the Clostrum protocol."
  :depends-on ("clostrum" "alexandria")
  :components
  ((:file "packages")
   (:file "simple"))
  :in-order-to ((test-op (load-op "clostrum-test")))
  :perform (test-op (operation component)
             (flet ((s (name) (uiop:find-symbol* name '#:clostrum/simple)))
               (uiop:symbol-call
                '#:clostrum/test '#:run-tests
                (s '#:simple-client)
                (s '#:simple-run-time-environment)
                nil
                (s '#:simple-compilation-environment)))))
