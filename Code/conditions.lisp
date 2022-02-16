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

(define-condition env:attempt-to-define-function-for-existing-special-operator
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a function,~@
                      but it already names a special operator."
                     (function-name condition)))))

(define-condition env:attempt-to-set-function-type-of-special-operator
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to set the function type of ~s~@
                      but it already names a special operator."
                     (function-name condition)))))

(define-condition env:attempt-to-set-function-type-of-macro
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to set the function type of ~s~@
                      but it already names a macro."
                     (function-name condition)))))

(define-condition env:attempt-to-declare-inline-a-non-existing-function
    (error)
  ((%function-name :initarg :function-name :reader function-name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to declare the function ~s INLINE~@
                      but there is no function with that name."
                     (function-name condition)))))

(define-condition env:attempt-to-define-constant-for-existing-constant
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant,~@
                      but it already names a constant."
                     (name condition)))))

(define-condition env:attempt-to-define-constant-for-existing-special-variable
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant,~@
                      but it already names a special variable."
                     (name condition)))))

(define-condition env:attempt-to-define-constant-for-existing-symbol-macro
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant,~@
                      but it already names a symbol macro."
                     (name condition)))))

(define-condition env:attempt-to-define-special-variable-for-existing-constant
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant,~@
                      but it already names a special variable."
                     (name condition)))))

(define-condition env:attempt-to-define-special-variable-for-existing-symbol-macro
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant,~@
                      but it already names a symbol macro."
                     (name condition)))))

(define-condition env:attempt-to-define-symbol-macro-for-existing-constant
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a symbol macro,~@
                      but it already names a constant."
                     (name condition)))))

(define-condition env:attempt-to-define-symbol-macro-for-existing-special-variable
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a symbol macro,~@
                      but it already names a special variable."
                     (name condition)))))
