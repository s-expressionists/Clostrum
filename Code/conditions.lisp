(cl:in-package #:clostrum-implementation)

(define-condition env:attempt-to-set-constant-value
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to set the value of ~s, but it names~@
                      a constant variable."
                     (name condition)))))

(define-condition env:attempt-to-redefine-constant-incompatibly
    (error)
  ((%name :initarg :name :reader name)
   (%old-value :initarg :old :reader old-value)
   (%new-value :initarg :new :reader new-value))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a constant with value ~s,~@
                      but it already names a constant with non-EQL value ~s."
                     (name condition)
                     (new-value condition) (old-value condition)))))

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
    (program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a symbol macro,~@
                      but it already names a constant."
                     (name condition)))))

(define-condition env:attempt-to-define-symbol-macro-for-existing-special-variable
    (program-error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define ~s as a symbol macro,~@
                      but it already names a special variable."
                     (name condition)))))

(define-condition env:attempt-to-define-a-setf-expander-of-non-existing-function-or-macro
    (error)
  ((%name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to define a SETF expander for ~s,~@
                      but the name is not defined as a function~@
                      or a macro."
                     (name condition)))))

(define-condition env:attempt-to-set-ftype-of-non-function (error)
  ((%name :initarg :name :reader name)
   (%status :initarg :status :reader status))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to proclaim an FTYPE for ~s,~@
                      but the name is defined as a ~s."
                     (name condition) (status condition)))))

(define-condition env:attempt-to-note-operator-as-function (error)
  ((%name :initarg :name :reader name)
   (%status :initarg :status :reader status))
  (:report (lambda (condition stream)
             (format stream
                     "Attempt to note ~s as a function,~@
                      but the name is already defined as a ~s."
                     (name condition) (status condition)))))

(define-condition env:undefined-class (cell-error)
  ()
  (:report (lambda (condition stream)
             (format stream "There is no class named ~s."
                     (cell-error-name condition)))))
