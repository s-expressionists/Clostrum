(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:sys #:clostrum-sys))
  (:shadow #:compiler-macro-function #:inline #:ftype #:optimize)
  (:export #:run-time-environment
           #:compilation-environment
           #:top-type)
  (:export #:make-operator-cell #:make-variable-cell #:make-type-cell))
