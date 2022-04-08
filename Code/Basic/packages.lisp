(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:shadow #:macro-function
           #:compiler-macro-function
           #:class)
  (:export #:run-time-environment
           #:evaluation-environment
           #:compilation-environment)
  (:export #:function-cell
           #:variable-cell))
