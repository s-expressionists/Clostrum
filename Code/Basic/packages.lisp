(cl:in-package #:common-lisp-user)

(defpackage #:clostrum-basic
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:sys #:clostrum-sys))
  (:shadow #:compiler-macro-function)
  (:export #:run-time-environment
           #:compilation-environment))
