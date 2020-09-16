(defpackage #:clostrum
  (:use)
  ;; protocol classes
  (:export
   #:run-time-environment
   #:evaluation-environment-mixin
   #:compilation-environment)
  ;; run-time operators
  (:export
   #:fboundp #:fmakunbound #:special-operator #:fdefinition #:macro-function
   #:compiler-macro-function #:function-type #:function-inline
   #:function-unbound #:function-description
   ;;
   #:boundp #:constant-variable #:special-variable #:symbol-macro
   #:variable-type #:variable-unbound #:variable-description
   ;;
   #:find-class #:class-description #:setf-expander #:type-expander
   #:find-package #:find-declaration)
  ;; compilation environment operators
  (:export
   #:function-description
   #:variable-description
   #:class-description)
  ;; shared readers
  (:export
   #:parent))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)))
