(cl:in-package #:common-lisp-user)

(defpackage #:clostrum/test
  (:use #:cl #:5am)
  (:local-nicknames (#:env #:clostrum))
  (:export #:run-tests))
