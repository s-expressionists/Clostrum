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
   #:attempt-to-define-special-operator-for-existing-macro)
  ;; Shared readers.
  (:export
   #:parent))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)))
