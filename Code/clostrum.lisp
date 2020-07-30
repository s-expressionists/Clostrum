(in-package #:clostrum-implementation)

(defgeneric parent (environment)
  (:documentation "Environment's parent environment."))

(defclass clostrum:run-time-environment () ()
  (:documentation "Base class for run-time environments."))

(defclass clostrum:evaluation-environment-mixin ()
  ((parent :initarg :parent :reader parent))
  (:default-initargs :parent (error "~s is required." :parent)))

(defclass clostrum:compilation-environment ()
  ((parent :initarg :parent :reader parent))
  (:default-initargs :parent (error "~s is required." :parent)))
