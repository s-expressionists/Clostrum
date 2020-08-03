(in-package #:clostrum-implementation)

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
