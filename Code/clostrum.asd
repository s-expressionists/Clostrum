(cl:in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :serial t
  :depends-on (#:documentation-utils)
  :components
  ((:file "packages")
   (:file "clostrum")
   (:file "default-methods")
   (:file "conditions")
   (:file "documentation")))
