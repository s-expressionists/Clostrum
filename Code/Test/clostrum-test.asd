(cl:in-package #:asdf-user)

(defsystem "clostrum-test"
  :description "Test suite for a Clostrum implementation."
  :depends-on ("clostrum"
               "clostrum-basic"
               "fiveam")
  :components
  ((:file "packages")
   (:file "tests"))
  :perform (test-op (operation component)
             (uiop:symbol-call
              '#:clostrum/test '#:run-tests
              nil ; client
              (find-symbol "RUN-TIME-ENVIRONMENT" (find-package "CLOSTRUM-BASIC")) ; runtime env
              nil ; evaluation env
              (find-symbol "COMPILATION-ENVIRONMENT" (find-package "CLOSTRUM-BASIC"))))) ; compilation env
