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
   #:compiler-macro-function #:function-type #:function-inline #:function-cell
   #:function-unbound #:function-lambda-list
   ;;
   #:boundp #:constant-variable #:special-variable #:symbol-macro
   #:variable-type #:variable-unbound #:find-class #:setf-expander
   #:default-setf-expander #:type-expander #:find-package)
  ;; compilation environment operators
  (:export
   #:function-lambda-list #:function-class-name #:method-class-name))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)))
