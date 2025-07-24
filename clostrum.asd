(cl:in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :homepage "https://s-expressionists.github.io/Clostrum/"
  :bug-tracker "https://github.com/s-expressionists/Clostrum/issues"
  :source-control (:git "https://github.com/s-expressionists/Clostrum.git")
  :serial t
  :depends-on (#:documentation-utils)
  :components
  ((:file "packages")
   (:file "clostrum")
   (:file "default-methods")
   (:file "conditions")
   (:file "documentation"))
  :in-order-to ((test-op (test-op "clostrum-test"))))
