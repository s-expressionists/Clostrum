(cl:in-package #:asdf-user)

(defsystem "clostrum-test"
  :description "Test suite for a Clostrum implementation."
  :depends-on ("clostrum" "fiveam")
  :components
  ((:file "packages")
   (:file "tests")))
