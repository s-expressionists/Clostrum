(defpackage #:clostrum/test
  (:use #:cl #:5am)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:export #:run-tests))

(cl:in-package #:clostrum/test)

(defvar *client-class*)
(defvar *run-time-environment-class*)
(defvar *evaluation-environment-class*)
(defvar *compilation-environment-class*)

(defun run-tests (*client-class*
                  *run-time-environment-class*
                  *evaluation-environment-class*
                  *compilation-environment-class*)
  (run! 'clostrum))

(def-suite* clostrum
  :description "A test suite for clostrum.")

;;; This fixture creates a constellation of environments. when a class of the
;;; evaluation environment is nil, then run-time and evaluation environments
;;; are assumed to be merged (and that is permissible by the spec). Otherwise
;;; a constellation of three environments is created.
;;;
;;; The fixture is an anaphoric macro introducing variables cli, renv, eenv
;;; and cenv which represent the client, run-time environment, evaluation
;;; environment and compilation environment.
(def-fixture with-envs ()
  (let* ((cli (make-instance *client-class*))
         (renv (make-instance *run-time-environment-class*))
         (eenv (if (not *evaluation-environment-class*)
                 renv
                 (make-instance *evaluation-environment-class* :parent renv)))
         (cenv (make-instance *compilation-environment-class* :parent eenv)))
    (declare (ignorable cli renv eenv cenv))
    (&body)))

;;; Smoke tests

(test empty-test
  (pass "Empty test."))

(test (create-environments :fixture with-envs)
  (pass "Environments created."))

;;; run-time-environment tests

(test (function-undefined :fixture with-envs)
  ;; Symbol without a function definition.
  (is (null (env:fboundp                   cli renv 'unknown)))
  (is (null (env:special-operator          cli renv 'unknown)))
  (is (null (env:macro-function            cli renv 'unknown)))
  (is (null (env:compiler-macro-function   cli renv 'unknown)))
  (finishes (env:fmakunbound               cli renv 'unknown))
  (multiple-value-bind (fun type)
      (finishes (env:fdefinition cli renv 'unknown))
    (is (eq type 'cl:undefined-function))
    (signals undefined-function (funcall fun))
    ;; Test whether the function which signals undefined-function may accept
    ;; arguments.
    (signals undefined-function (funcall fun :lambda 30 :answer 42)))
  ;; Proclamations
  (is (null (env:function-type cli renv 'unknown)))
  (is (null (env:function-inline cli renv 'unknown)))
  (is (null (env:function-description cli renv 'unknown))))

(test (symbol-macro-not-boundp :fixture with-envs)
  (finishes (setf (env:symbol-macro cli renv 'symbol-macro) 42))
  (is (not (env:boundp cli renv 'symbol-macro))))
