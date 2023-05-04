(cl:in-package #:asdf-user)

(defsystem "clostrum"
  :description "First-class global environments for Common Lisp."
  :serial t
  :components
  ((:file "packages")
   (:file "clostrum")
   (:file "default-methods")
   (:file "conditions")))
