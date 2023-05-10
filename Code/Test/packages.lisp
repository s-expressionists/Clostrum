(cl:in-package #:common-lisp-user)

(defpackage #:clostrum/test
  (:use #:cl #:5am)
  (:local-nicknames (#:env #:clostrum)
                    (#:sys #:clostrum-sys))
  (:export #:run-tests #:run-tests-exit))
