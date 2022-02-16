(defpackage #:clostrum
  (:use)
  ;; Protocol classes.
  (:export
   #:run-time-environment
   #:evaluation-environment-mixin
   #:compilation-environment)
  ;; Run-time operators.
  (:export
   #:fboundp #:fmakunbound #:special-operator #:fdefinition #:macro-function
   #:compiler-macro-function #:function-type #:function-inline
   #:function-unbound #:function-description
   ;;
   #:boundp #:constant-variable #:special-variable #:symbol-macro
   #:variable-type #:variable-unbound #:variable-description
   ;;
   #:find-class #:class-description #:setf-expander #:type-expander
   #:find-package #:proclamation)
  ;; Compilation environment operators.
  (:export
   #:function-description
   #:variable-description
   #:class-description)
  ;; Condition types.
  (:export
   #:attempt-to-define-special-operator-for-existing-function
   #:attempt-to-define-special-operator-for-existing-macro
   #:attempt-to-define-function-for-existing-special-operator
   #:attempt-to-set-function-type-of-special-operator
   #:attempt-to-set-function-type-of-macro
   #:attempt-to-declare-inline-a-non-existing-function
   #:attempt-to-define-constant-for-existing-constant
   #:attempt-to-define-constant-for-existing-special-variable
   #:attempt-to-define-constant-for-existing-symbol-macro
   #:attempt-to-define-special-variable-for-existing-constant
   #:attempt-to-define-special-variable-for-existing-symbol-macro
   #:attempt-to-define-symbol-macro-for-existing-constant)
  ;; Shared readers.
  (:export
   #:parent))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)))
