(cl:in-package #:clostrum)

(cl:define-condition attempt-to-define-special-operator-for-existing-function
    (cl:error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (cl:lambda (condition stream)
             (cl:format stream
                        "Attempt to define ~s as a special operator,~@
                         but it already names a function."
                        (function-name condition)))))
