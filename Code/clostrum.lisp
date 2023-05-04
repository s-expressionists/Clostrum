(cl:in-package #:clostrum-implementation)

(defmacro define-accessor (name lambda-list &rest options)
  `(progn (defgeneric ,name ,lambda-list ,@options)
          (defgeneric (setf ,name) (new-value ,@lambda-list) ,@options)))

;;; System API

;;; Run-time environment.

(define-accessor sys:operator-status (client environment operator-name))
(defgeneric sys:operator-cell (client environment operator-name))
(define-accessor sys:compiler-macro-function (client environment operator-name))
(define-accessor sys:setf-expander (client environment operator-name))
(define-accessor sys:operator-cell-value (client cell))
(defgeneric sys:operator-cell-boundp (client cell))
(defgeneric sys:operator-cell-makunbound (client cell))

(define-accessor sys:variable-status (client environment variable-name))
(defgeneric sys:variable-cell (client environment variable-name))
(define-accessor sys:variable-macro-expander
    (client environment variable-name))
(define-accessor sys:variable-cell-value (client cell))
(defgeneric sys:variable-cell-boundp (client cell))
(defgeneric sys:variable-cell-makunbound (client cell))

(defgeneric sys:type-cell (client environment type-name))
(define-accessor sys:type-expander (client environment type-name))
(define-accessor sys:type-cell-value (client cell))
(defgeneric sys:type-cell-boundp (client cell))
(defgeneric sys:type-cell-makunbound (client cell))

(define-accessor sys:find-package (client environment name))
(define-accessor sys:proclamation (client environment name))

;;; Compilation environment.

(define-accessor sys:function-description (client environment function-name))
(define-accessor sys:variable-description (client environment variable-name))
(define-accessor sys:type-description (client environment type-name))

;;; High level API

(define-accessor env:fdefinition (client environment operator-name))
(defgeneric env:fboundp (client environment operator-name))
(defgeneric env:fmakunbound (client environment operator-name))
(define-accessor env:macro-function (client environment operator-name))
(define-accessor env:setf-expander (client environment operator-name))

(define-accessor env:symbol-value (client environment variable-name))
(defgeneric env:boundp (client env variable-name))
(defgeneric env:makunbound (client env variable-name))
(defgeneric env:make-variable (client environment variable-name value))
(defgeneric env:make-parameter (client environment variable-name value))
(defgeneric env:make-constant (client environment variable-name value))
(defgeneric env:make-symbol-macro (client environment variable-name expansion))

(define-accessor env:find-class
    (client environment class-name &optional errorp))
(defgeneric env:make-type (client environment type-name expander))
(defgeneric env:type-expand-1 (client environment type-specifier))
(defgeneric env:type-expand (client environment type-expand))

(defgeneric env:macroexpand-1 (client environment form))
(defgeneric env:macroexpand (client environment form))
(defgeneric env:constantp (client environment form))
