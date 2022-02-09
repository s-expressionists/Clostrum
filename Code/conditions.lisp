(cl:in-package #:clostrum)

(cl:define-condition attempt-to-define-special-operator-for-existing-function
    (cl:error)
  ((%function-name :initarg :function-name :reader function-name)))
