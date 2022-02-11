(cl:in-package #:clostrum-implementation)

(define-condition attempt-to-define-special-operator-for-existing-function
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a special operator,~@
                      but it already names a function."
                     (function-name condition)))))
