(cl:in-package #:asdf-user)

(defsystem "clostrum-trucler"
  :description "Implementation of Trucler methods for use with Clostrum."
  :depends-on ("clostrum" "trucler-base")
  :components
  ((:file "packages")
   (:file "trucler")))
