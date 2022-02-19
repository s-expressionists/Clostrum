(cl:in-package #:common-lisp-user)

(defpackage #:clostrum/test
  (:use #:cl #:5am)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:export #:run-tests))
