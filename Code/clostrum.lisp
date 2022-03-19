(cl:in-package #:clostrum-implementation)

(defgeneric env:parent (environment)
  (:documentation "Environment's parent environment."))

(defclass env:run-time-environment () ()
  (:documentation "Base class for run-time environments."))

(defclass env:evaluation-environment-mixin ()
  ((parent :initarg :parent :reader env:parent))
  (:default-initargs :parent (error "~s is required." :parent)))

(defclass env:compilation-environment ()
  ((parent :initarg :parent :reader env:parent))
  (:default-initargs :parent (error "~s is required." :parent)))

;;; Macros DEFINE-FUNCTION and DEFINE-ACCESSOR are used to define
;;; run-time-environment protocol functions, so it is possible to generate
;;; trampolines for the evaluation-environment-mixin automatically. They also
;;; ensure that each protocol function has parameters environment and client.

(eval-when (:compile-toplevel)
  (defparameter *run-time-operators* nil)
  (defparameter *run-time-accessors* nil)
  (defparameter *compilation-operators* nil))

(defmacro define-operator (name args &rest options)
  (assert (member 'environment args))
  (assert (member 'client args))
  `(progn (defgeneric ,name ,args ,@options)
          (eval-when (:compile-toplevel)
            (push (list* ',name ',args) *run-time-operators*))))

(defmacro define-accessor (name args &rest options)
  (assert (member 'environment args))
  (assert (member 'client args))
  (let ((new-value (gensym "NEW-VALUE")))
    `(progn (defgeneric ,name ,args ,@options)
            (defgeneric (setf ,name) (,new-value ,@args) ,@options)
            (eval-when (:compile-toplevel)
              (push (list* ',name ',args) *run-time-accessors*)))))

(defmacro define-operator* (name args &rest options)
  (assert (member 'environment args))
  (assert (member 'client args))
  `(progn (defgeneric ,name (,@args) ,@options)
          (eval-when (:compile-toplevel)
            (push (list* ',name ',args) *compilation-operators*))))

;;; run time
(define-operator env:fboundp (client environment function-name))
(define-operator env:fmakunbound (client environment function-name))
(define-accessor env:special-operator (client environment function-name))
(define-accessor env:fdefinition (client environment function-name))
(define-accessor env:macro-function (client environment symbol))
(define-accessor env:compiler-macro-function (client environment function-name))
(define-accessor env:function-type (client environment function-name))
(define-accessor env:function-inline (client environment function-name))
(define-operator env:function-unbound (client environment function-name))
(define-operator env:map-defined-functions (client environment function))
(define-operator env:map-defined-classes (client environment function))

(define-operator env:boundp (client environment symbol))
(define-accessor env:constant-variable (client environment symbol))
;;; The accessor ENV:SPECIAL-VARIABLE is defined as two operators, because it
;;; has an irregular lambda list.
(define-operator env:special-variable (client environment symbol))
(define-operator (setf env:special-variable)
    (new-value client environment symbol init-p))
(define-accessor env:symbol-macro (client environment symbol))
(define-accessor env:variable-type (client environment symbol))
(define-operator env:variable-unbound (client environment symbol))

(define-accessor env:find-class (client environment symbol))
(define-accessor env:setf-expander (client environment symbol))
(define-accessor env:type-expander (client environment symbol))
(define-accessor env:find-package (client environment name))
(define-accessor env:proclamation (client environment name))

(define-operator env:function-description (client environment function-name))
(define-operator env:variable-description (client environment symbol))
(define-operator env:class-description (client environment symbol))

;;; compilation time
(define-operator* (setf env:function-description)
    (new-value client environment funciton-name))

(define-operator* (setf env:variable-description)
    (new-value client environment symbol))

(define-operator* (setf env:class-description)
    (new-value client environment symbol))

(defmethod env:function-description
    (client (environment env:compilation-environment) function-name)
  (env:function-description client (env:parent environment) function-name))

(defmethod env:variable-description
    (client (environment env:compilation-environment) symbol)
  (env:variable-description client (env:parent environment) symbol))

(defmethod env:class-description
    (client (environment env:compilation-environment) symbol)
  (env:class-description client (env:parent environment) symbol))

(defmethod env:macro-function
    (client (environment env:compilation-environment) symbol)
  (env:macro-function client (env:parent environment) symbol))

(defmethod (setf env:macro-function)
    (function client (environment env:compilation-environment) symbol)
  (funcall #'(setf env:macro-function) function
           client (env:parent environment) symbol))

(defmethod env:fdefinition
    (client (environment env:compilation-environment) symbol)
  (env:fdefinition client (env:parent environment) symbol))

(defmethod (setf env:fdefinition)
    (function client (environment env:compilation-environment) symbol)
  (funcall #'(setf env:fdefinition) function
           client (env:parent environment) symbol))

(defmethod env:proclamation
    (client (environment env:compilation-environment) symbol)
  (env:proclamation client (env:parent environment) symbol))

(defmethod (setf env:proclamation)
    (function client (environment env:compilation-environment) symbol)
  (funcall #'(setf env:proclamation) function
           client (env:parent environment) symbol))

;;; evaluation-environment-mixin trampolines
;;;
;;; To minimize possibility of a typo trampolines are generated
;;; automatically. That also reduce amount of code in the module.
(defmacro define-operator-trampolines (env-class)
  (let ((methods
          (loop with spec = `(environment ,env-class)
                for (name . arg-names) in *run-time-operators*
                collect
                `(defmethod ,name ,(subst spec 'environment arg-names)
                   (let ((environment (env:parent environment)))
                     (funcall (function ,name) ,@arg-names))))))
    `(progn ,@methods)))

(defmacro define-accessor-trampolines (env-class)
  (let ((methods
          (loop with spec = `(environment ,env-class)
                with nval = (gensym "NEW-VALUE")
                for (name . arg-names) in *run-time-accessors*
                collect
                `(defmethod ,name ,(subst spec 'environment arg-names)
                   (let ((environment (env:parent environment)))
                     (funcall (function ,name) ,@arg-names)))
                collect
                `(defmethod (setf ,name)
                     ,(list* nval (subst spec 'environment arg-names))
                   (let ((environment (env:parent environment)))
                     (funcall (function (setf ,name)) ,nval ,@arg-names))))))
    `(progn ,@methods)))

(define-operator-trampolines env:evaluation-environment-mixin)
(define-accessor-trampolines env:evaluation-environment-mixin)
