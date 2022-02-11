(cl:in-package #:clostrum-implementation)

(define-condition env:attempt-to-define-special-operator-for-existing-function
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a special operator,~@
                      but it already names a function."
                     (function-name condition)))))

(define-condition env:attempt-to-define-special-operator-for-existing-macro
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a special operator,~@
                      but it already names a macro."
                     (function-name condition)))))
