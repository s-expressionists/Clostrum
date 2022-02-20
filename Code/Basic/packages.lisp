(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:shadow #:class-name #:package-name
           #:macro-function #:compiler-macro-function)
  (:export #:run-time-environment
           #:evaluation-environment
           #:compilation-environment)
  (:export #:function-cell
           #:variable-cell))
