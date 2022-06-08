(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:shadow #:macro-function
           #:fdefinition
           #:compiler-macro-function
           #:class)
  (:export #:run-time-environment
           #:compilation-environment))
