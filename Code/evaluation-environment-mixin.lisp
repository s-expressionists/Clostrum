(cl:in-package #:clostrum-implementation)

(defclass env:evaluation-environment-mixin ()
  ((parent :initarg :parent :reader env:parent))
  (:default-initargs :parent (error "~s is required." :parent)))

(defmethod env:special-operator
    (client (environment env:evaluation-environment-mixin) name)
  (let ((result (call-next-method)))
    (if (null result)
        (env:special-operator client (env:parent environment) name)
        result)))

(defmethod env:fdefinition
    (client (environment env:evaluation-environment-mixin) name)
  (let ((result (call-next-method)))
    (if (null result)
        (env:fdefinition client (env:parent environment) name)
        result)))

(defmethod env:macro-function
    (client (environment env:evaluation-environment-mixin) name)
  (let ((result (call-next-method)))
    (if (null result)
        (env:macro-function client (env:parent environment) name)
        result)))

(defmethod env:compiler-macro-function
    (client (environment env:evaluation-environment-mixin) name)
  (let ((result (call-next-method)))
    (if (null result)
        (env:compiler-macro-function client (env:parent environment) name)
        result)))

(defmethod env:function-type
    (client (environment env:evaluation-environment-mixin) name)
  (let ((result (call-next-method)))
    (if (null result)
        (env:function-type client (env:parent environment) name)
        result)))
