(cl:in-package #:common-lisp-user)

(defpackage #:clostrum/simple
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:shadow #:class-name #:package-name)
  (:export #:simple-client
           #:simple-run-time-environment
           #:simple-evaluation-environment
           #:simple-compilation-environment))
