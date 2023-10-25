(cl:in-package #:clostrum-implementation)

(defclass env:environment () ())
(defclass env:run-time-environment (env:environment) ())
(defclass env:compilation-environment (env:environment) ())

(defmacro define-accessor (name lambda-list &rest options)
  `(progn (defgeneric ,name ,lambda-list ,@options)
          (defgeneric (setf ,name) (new-value ,@lambda-list) ,@options)))

;;; System API

(defgeneric sys:parent (client environment))

;;; Run-time environment.

(define-accessor sys:operator-status (client environment operator-name))
(defgeneric sys:operator-cell (client environment operator-name))
(defgeneric sys:ensure-operator-cell (client environment operator-name))
(define-accessor sys:compiler-macro-function (client environment operator-name))
(define-accessor sys:setf-expander (client environment operator-name))
(define-accessor sys:operator-inline (client environment operator-name))
(defgeneric sys:operator-inline-known-p (client environment operator-name))
(define-accessor sys:operator-inline-data (client environment operator-name))
(define-accessor sys:operator-ftype (client environment operator-name))

(define-accessor sys:operator-cell-value (client cell))
(defgeneric sys:operator-cell-boundp (client cell))
(defgeneric sys:operator-cell-makunbound (client cell))

(define-accessor sys:variable-status (client environment variable-name))
(defgeneric sys:variable-cell (client environment variable-name))
(defgeneric sys:ensure-variable-cell (client environment variable-name))
(define-accessor sys:variable-macro-expander
    (client environment variable-name))
(define-accessor sys:variable-type (client environment variable-name))

(define-accessor sys:variable-cell-value (client cell))
(defgeneric sys:variable-cell-boundp (client cell))
(defgeneric sys:variable-cell-makunbound (client cell))

(define-accessor sys:symbol-plist (client environment symbol))
(defgeneric sys:symbol-plist-known-p (client environment symbol))

(defgeneric sys:type-cell (client environment type-name))
(defgeneric sys:ensure-type-cell (client environment type-name))
(define-accessor sys:type-expander (client environment type-name))
(define-accessor sys:type-cell-value (client cell))
(defgeneric sys:type-cell-boundp (client cell))
(defgeneric sys:type-cell-makunbound (client cell))

(define-accessor sys:find-package (client environment name))
(define-accessor sys:package-name (client environment package))
(defgeneric sys:map-all-packages (client environment function))
(defgeneric sys:package-names (client environment package))
(define-accessor sys:proclamation (client environment name))
(define-accessor sys:optimize (client environment))

;;; High level API

(defgeneric env:merge-types (client type1 type2))
(defgeneric env:merge-optimize (client new-optimize old-optimize))

(defgeneric env:operator-status (client environment operator-name))
(defgeneric env:ensure-operator-cell (client environment operator-name))
(define-accessor env:fdefinition (client environment operator-name))
(defgeneric env:fboundp (client environment operator-name))
(defgeneric env:fmakunbound (client environment operator-name))
(defgeneric env:special-operator-p (client environment operator-name))
(defgeneric env:make-special-operator (client environment operator-name new))
(defgeneric env:note-function (client environment operator-name))
(define-accessor env:operator-inline (client environment operator-name))
(define-accessor env:operator-ftype (client environment operator-name))
(define-accessor env:macro-function (client environment operator-name))
(define-accessor env:compiler-macro-function (client environment operator-name))
(define-accessor env:setf-expander (client environment operator-name))

(defgeneric env:variable-status (client environment variable-name))
(defgeneric env:ensure-variable-cell (client environment variable-name))
(define-accessor env:symbol-value (client environment variable-name))
(defgeneric env:boundp (client env variable-name))
(defgeneric env:makunbound (client env variable-name))
(define-accessor env:variable-type (client environment variable-name))
(defgeneric env:make-variable (client environment variable-name
                               &optional value))
(defgeneric env:make-parameter (client environment variable-name value))
(defgeneric env:make-constant (client environment variable-name value))
(defgeneric env:make-symbol-macro (client environment variable-name expansion))
(define-accessor env:symbol-plist (client environment symbol))

(defgeneric env:ensure-type-cell (client environment name))
(define-accessor env:find-class
    (client environment class-name &optional errorp))
(define-accessor env:type-expander (client environment type-name))
(defgeneric env:type-expand-1 (client environment type-specifier))
(defgeneric env:type-expand (client environment type-expand))

(define-accessor env:find-package (client environment name))
(define-accessor env:package-name (client environment package))
(defgeneric env:map-all-packages (client environment function))
(defgeneric env:package-names (client environment package))

(defgeneric env:optimize (client environment))
(defgeneric env:proclaim-optimize (client environment optimize))

(defgeneric env:macroexpand-1 (client environment form))
(defgeneric env:macroexpand (client environment form))
(defgeneric env:constantp (client environment form))
