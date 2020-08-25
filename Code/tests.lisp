(defpackage #:clostrum/test
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:export #:run-tests))

(in-package #:clostrum/test)

(defvar *client-class*)
(defvar *run-time-environment-class*)
(defvar *evaluation-environment-class*)
(defvar *compilation-environment-class*)

(defun run-tests (*client-class*
                  *run-time-environment-class*
                  *evaluation-environment-class*
                  *compilation-environment-class*)
  (5am:run! 'clostrum))

(5am:def-suite* clostrum
  :description "A test suite for clostrum.")

;;; This fixture creates a constellation of environments. when a class of the
;;; evaluation environment is nil, then run-time and evaluation environments
;;; are assumed to be merged (and that is permissible by the spec). Otherwise
;;; a constellation of three environments is created.
;;;
;;; The fixture is an anaphoric macro introducing variables ^v, ^r, ^e and ^c.
(5am:def-fixture with-envs ()
  (let* ((^v (make-instance *client-class*))
         (^r (make-instance *run-time-environment-class*))
         (^e (if (not *evaluation-environment-class*)
                 ^r
                 (make-instance *evaluation-environment-class* :parent ^r)))
         (^c (make-instance *compilation-environment-class* :parent ^e)))
    (declare (ignorable ^v ^r ^e ^c))
    (&body)))

;;; Smoke tests

(5am:test empty-test
  (5am:pass "Empty test."))

(5am:test (create-environments :fixture with-envs)
  (5am:pass "Environments created."))

;;; run-time-environment tests

(5am:test (crud-function :fixture with-envs)
  (5am:is (null (env:fboundp ^v ^r 'function-1))))
