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
;;; The fixture is an anaphoric macro introducing variables cli, renv, eenv
;;; and cenv which represent the client, run-time environment, evaluation
;;; environment and compilation environment.
(5am:def-fixture with-envs ()
  (let* ((cli (make-instance *client-class*))
         (renv (make-instance *run-time-environment-class*))
         (eenv (if (not *evaluation-environment-class*)
                 renv
                 (make-instance *evaluation-environment-class* :parent renv)))
         (cenv (make-instance *compilation-environment-class* :parent eenv)))
    (declare (ignorable cli renv eenv cenv))
    (&body)))

;;; Smoke tests

(5am:test empty-test
  (5am:pass "Empty test."))

(5am:test (create-environments :fixture with-envs)
  (5am:pass "Environments created."))

;;; run-time-environment tests

(5am:test (function-undefined :fixture with-envs)
  ;; Symbol without a function definition.
  (5am:is (null (env:fboundp                   cli renv 'unknown)))
  (5am:is (null (env:special-operator          cli renv 'unknown)))
  (5am:is (null (env:macro-function            cli renv 'unknown)))
  (5am:is (null (env:compiler-macro-function   cli renv 'unknown)))
  (5am:finishes (env:fmakunbound               cli renv 'unknown))
  (multiple-value-bind (fun type)
      (5am:finishes (env:fdefinition cli renv 'unknown))
    (5am:is (eq type 'cl:undefined-function))
    (5am:signals undefined-function (funcall fun))
    ;; Test whether the function which signals undefined-function may accept
    ;; arguments.
    (5am:signals undefined-function (funcall fun :lambda 30 :answer 42)))
  ;; Proclamations
  (5am:is (null (env:function-type cli renv 'unknown)))
  (5am:is (null (env:function-inline cli renv 'unknown)))
  (5am:is (null (env:function-description cli renv 'unknown))))

(5am:test (symbol-macro-not-boundp :fixture with-envs)
  (5am:finishes (setf (env:symbol-macro cli renv 'symbol-macro) 42))
  (5am:is (not (env:boundp cli renv 'symbol-macro))))
