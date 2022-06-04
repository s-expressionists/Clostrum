(cl:in-package #:clostrum-implementation)

(defclass env:compilation-environment ()
  ((parent :initarg :parent :reader env:parent))
  (:default-initargs :parent (error "~s is required." :parent)))

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
