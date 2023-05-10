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
  (let* ((cli (if *client-class* (make-instance *client-class*) nil))
         (renv (make-instance *run-time-environment-class*))
         (eenv (if (not *evaluation-environment-class*)
                 renv
                 (make-instance *evaluation-environment-class* :parent renv)))
         (cenv (make-instance *compilation-environment-class* :parent eenv)))
    (declare (ignorable cli renv eenv cenv))
    (&body)))

;;; Smoke tests

(test (create-environments :fixture with-envs)
  (pass "Environments created."))

;;; run-time-environment tests

;;; Operators

(test (function-undefined-sys :fixture with-envs)
  (let ((fname (make-symbol "F")))
    (is (null (sys:operator-status cli renv fname)))
    (finishes (sys:operator-cell cli renv fname))
    (is-false (sys:operator-cell-boundp
               cli (sys:operator-cell cli renv fname)))))

;;; Check that we can call an undefined function's cell value.
;;; This is a separate test because it's a stronger requirement
;;; on implementations than the cell merely existing and being boundp.
(test (call-undefined-function :fixture with-envs)
  (let* ((fname (make-symbol "F"))
         (cell (sys:operator-cell cli renv fname)))
    (signals undefined-function (funcall (sys:operator-cell-value cli cell)))))

(test (function-binding :fixture with-envs)
  (let ((fname (make-symbol "F")))
    (is-false (env:fboundp cli renv fname))
    (signals undefined-function (env:fdefinition cli renv fname))
    (setf (env:fdefinition cli renv fname) #'identity)
    (is (eql :function (sys:operator-status cli renv fname)))
    (let ((cell (sys:operator-cell cli renv fname)))
      (is-true (sys:operator-cell-boundp cli cell))
      (is (eql #'identity (sys:operator-cell-value cli cell))))
    (is (eql #'identity (env:fdefinition cli renv fname)))
    (is (null (env:macro-function cli renv fname)))
    (env:fmakunbound cli renv fname)
    (is-false (env:fboundp cli renv fname))
    (is (null (sys:operator-status cli renv fname)))))

(test (macro-binding :fixture with-envs)
  (let ((mname (make-symbol "M"))
        (expander
          (lambda (form env)
            (declare (ignore env))
            (destructuring-bind (op lambda-list . body) form
              (declare (ignore op))
              `(function (lambda ,lambda-list ,@body))))))
    (is (null (env:macro-function cli renv mname)))
    (setf (env:macro-function cli renv mname) expander)
    (is (eql :macro (sys:operator-status cli renv mname)))
    (is-true (env:fboundp cli renv mname))
    (is (eql expander (env:macro-function cli renv mname)))
    (is (equal '(function (lambda (x) x))
               (env:macroexpand-1 cli renv `(,mname (x) x))))
    (env:fmakunbound cli renv mname)
    (is-false (env:fboundp cli renv mname))))

(test (special-operator-binding :fixture with-envs)
  (let ((sname (make-symbol "S"))
        (op #'identity))
    (env:make-special-operator cli renv sname op)
    (is-true (env:fboundp cli renv sname))
    (is (null (env:macro-function cli renv sname)))
    (is-true (env:special-operator-p cli renv sname))
    (is (eql op (env:fdefinition cli renv sname)))))

(test (compiler-macro-function :fixture with-envs)
  (let ((cname (make-symbol "C"))
        (expander (lambda (form env) (declare (ignore env)) form)))
    (setf (env:fdefinition cli renv cname) #'identity)
    (is (null (env:compiler-macro-function cli renv cname)))
    (setf (env:compiler-macro-function cli renv cname) expander)
    (is (eql expander (env:compiler-macro-function cli renv cname)))
    (env:fmakunbound cli renv cname)))

(test (setf-expander :fixture with-envs)
  (let ((sname (make-symbol "S"))
        (expander (lambda (form env)
                    (declare (ignore env))
                    (let ((store (gensym "STORE")))
                      (values () () `(,store) `(setq ,form ,store) form)))))
    (setf (env:fdefinition cli renv sname) #'identity)
    (is (null (env:setf-expander cli renv sname)))
    (setf (env:setf-expander cli renv sname) expander)
    (is (eql expander (env:setf-expander cli renv sname)))))

;;; Variables

(test (variable-undefined-sys :fixture with-envs)
  (let ((vname (make-symbol "V")))
    (is (null (sys:variable-status cli renv vname)))
    (finishes (sys:variable-cell cli renv vname))
    (is-false (sys:variable-cell-boundp
               cli (sys:operator-cell cli renv vname)))))

(test (symbol-value :fixture with-envs)
  (let ((vname (make-symbol "V"))
        (value (list nil)))
    (is-false (env:boundp cli renv vname))
    (signals unbound-variable (env:symbol-value cli renv vname))
    (setf (env:symbol-value cli renv vname) value)
    (is-true (env:boundp cli renv vname))
    ;; Make sure that the symbol value being set
    ;; does not proclaim the variable special.
    (is (null (sys:variable-status cli renv vname)))
    (is (eql value (env:symbol-value cli renv vname)))
    (env:makunbound cli renv vname)
    (is-false (env:boundp cli renv vname))
    (signals unbound-variable (env:symbol-value cli renv vname))))

(test (make-variable :fixture with-envs)
  (let ((vname (make-symbol "V"))
        (value (list nil)))
    (finishes (env:make-variable cli renv vname value))
    (is-true (env:boundp cli renv vname))
    (is (eql :special (sys:variable-status cli renv vname)))
    (is (eql value (env:symbol-value cli renv vname)))
    (env:make-variable cli renv vname (list nil))
    (is (eql value (env:symbol-value cli renv vname)))))

(test (make-parameter :fixture with-envs)
  (let ((vname (make-symbol "V"))
        (value (list nil)))
    (finishes (env:make-parameter cli renv vname value))
    (is-true (env:boundp cli renv vname))
    (is (eql :special (sys:variable-status cli renv vname)))
    (is (eql value (env:symbol-value cli renv vname)))
    (let ((new-value (list nil)))
      (finishes (env:make-parameter cli renv vname new-value))
      (is (eql new-value (env:symbol-value cli renv vname))))))

(test (make-constant :fixture with-envs)
  (let ((cname (make-symbol "C"))
        (value (list nil)))
    (finishes (env:make-constant cli renv cname value))
    (is-true (env:boundp cli renv cname))
    (is (eql :constant (sys:variable-status cli renv cname)))
    (is-true (env:constantp cli renv cname))
    (is (eql value (env:symbol-value cli renv cname)))
    (finishes (env:make-constant cli renv cname value))
    (is (eql value (env:symbol-value cli renv cname)))))

(test (make-symbol-macro :fixture with-envs)
  (let ((sname (make-symbol "S"))
        (expansion (make-symbol "EXPANSION")))
    (finishes (env:make-symbol-macro cli renv sname expansion))
    (is (eql :symbol-macro (sys:variable-status cli renv sname)))
    (is-false (env:boundp cli renv sname))
    (is (eql expansion (env:macroexpand-1 cli renv sname)))))

;;; Check that symbol macrology is independent from its value.
(test (symbol-macro-value :fixture with-envs)
  (let ((sname (make-symbol "S"))
        (expansion (make-symbol "EXPANSION"))
        (value (list nil)))
    (setf (env:symbol-value cli renv sname) value)
    (finishes (env:make-symbol-macro cli renv sname expansion))
    (is-true (env:boundp cli renv sname))
    (is (eql value (env:symbol-value cli renv sname)))
    (is (eql expansion (env:macroexpand-1 cli renv sname)))))

;;; DEFINE-SYMBOL-MACRO is specified to signal errors in some cases.
;;; (defvar, defparameter, and defconstant are not, so no tests there)
(test (symbol-macro-redef-variable-error :fixture with-envs)
  (let ((vname (make-symbol "VARIABLE")))
    (env:make-variable cli renv vname (list nil))
    (signals program-error (env:make-symbol-macro cli renv vname (gensym)))))

(test (symbol-macro-redef-constant-error :fixture with-envs)
  (let ((vname (make-symbol "CONSTANT")))
    (env:make-constant cli renv vname (list nil))
    (signals program-error (env:make-symbol-macro cli renv vname (gensym)))))

;;; Types

(test (class-unbound-sys :fixture with-envs)
  (let ((cname (make-symbol "C")))
    (finishes (sys:type-cell cli renv cname))
    (is-false (sys:type-cell-boundp
               cli (sys:type-cell cli renv cname)))))

(test (make-class :fixture with-envs)
  (let ((cname (make-symbol "C"))
        (class (cl:find-class 'cons)))
    (finishes (setf (env:find-class cli renv cname) class))
    (finishes (env:find-class cli renv cname))
    (is (eql class (env:find-class cli renv cname t)))))

(test (class-unbound-error :fixture with-envs)
  (let ((cname (make-symbol "C")))
    (signals error (env:find-class cli renv cname))
    (is (null (env:find-class cli renv cname nil)))))

(test (make-type :fixture with-envs)
  (let ((tname (make-symbol "T"))
        (expander (lambda (specifier env)
                    (declare (ignore specifier env))
                    '(or cons null))))
    (finishes (env:make-type cli renv tname expander))
    (is (eql expander (env:type-expander cli renv tname)))
    (is (equal '(or cons null) (env:type-expand-1 cli renv tname)))
    (is (equal '(or cons null) (env:type-expand-1 cli renv `(,tname))))))
