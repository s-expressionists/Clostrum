(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum))
  (:shadow #:macro-function
           #:compiler-macro-function
           #:class)
  (:export #:run-time-environment
           #:compilation-environment))
