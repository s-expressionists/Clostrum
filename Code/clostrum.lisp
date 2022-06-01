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
;;; run-time-environment protocol functions, so it is possible to
;;; generate trampolines for the evaluation-environment-mixin
;;; automatically.

(defmacro define-operator (name lambda-list &rest options)
  (let* ((mixin-class-name 'env:evaluation-environment-mixin)
         (specialized-parameter `(environment ,mixin-class-name))
         (method-lambda-list
           (subst specialized-parameter 'environment lambda-list)))
    `(progn (defgeneric ,name ,lambda-list ,@options)
            (defmethod ,name ,method-lambda-list
              (let ((environment (env:parent environment)))
                (funcall (function ,name) ,@lambda-list))))))

(defmacro define-accessor (name lambda-list &rest options)
  (let* ((mixin-class-name 'env:evaluation-environment-mixin)
         (specialized-parameter `(environment ,mixin-class-name))
         (method-lambda-list
           (subst specialized-parameter 'environment lambda-list))
         (new-value (gensym "NEW-VALUE")))
    `(progn (defgeneric ,name ,lambda-list ,@options)
            (defgeneric (setf ,name) (,new-value ,@lambda-list) ,@options)
            (defmethod ,name ,method-lambda-list
              (let ((environment (env:parent environment)))
                (,name ,@lambda-list)))
            (defmethod (setf ,name) (,new-value ,@method-lambda-list)
              (let ((environment (env:parent environment)))
                (setf (,name ,@lambda-list) ,new-value))))))

(defmacro define-operator* (name args &rest options)
  `(defgeneric ,name (,@args) ,@options))

;;; run time
(define-accessor env:special-operator (client environment function-name))
(define-accessor env:fdefinition (client environment function-name))
(define-accessor env:macro-function (client environment symbol))
(define-accessor env:compiler-macro-function (client environment function-name))
(define-accessor env:function-type (client environment function-name))
(define-accessor env:function-inline (client environment function-name))
(define-operator env:map-defined-functions (client environment function))
(define-operator env:map-defined-classes (client environment function))

(define-accessor env:constant-variable (client environment symbol))
;;; The accessor ENV:SPECIAL-VARIABLE is defined as two operators,
;;; because it has an irregular lambda list.
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

;;; Compilation time.
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

(defgeneric env:import-function (client from-environment name to-environment))

;;; A call to this function always succeeds.  It returns a CONS cell,
;;; of which the CDR slot contains a function that, when called,
;;; signals an error of type UNDEFINED-FUNCTION.  If FUNCTION-NAME has
;;; no definition as a function, then the CAR slot of this cell
;;; contains the same function object as is contained in the CDR slot.
;;; If FUNCTION-NAME has a definition as a function, then the CAR slot
;;; of this cell contains the defined function object.  The return
;;; value of this function is always the same (in the sense of EQ)
;;; when it is passed the same (in the sense of EQUAL) function name
;;; and the same (in the sense of EQ) environment.
(defgeneric env:function-cell (client environment function-name))

;;; A call to this function always succeeds. It returns a CONS cell,
;;; in which the CAR always holds the current definition of the
;;; variable named SYMBOL.  When SYMBOL has no definition as a
;;; variable, the CAR of this cell will contain an object that
;;; indicates that the variable is unbound. This object is the return
;;; value of the function VARIABLE-UNBOUND. The return value of this
;;; function is always the same (in the sense of EQ) when it is passed
;;; the same symbol and the same environment.
(defgeneric env:variable-cell (client environment symbol))
