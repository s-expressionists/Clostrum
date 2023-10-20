;;; Low level API.
(defpackage #:clostrum-sys
  (:use #:cl)
  (:shadow #:compiler-macro-function #:find-package #:symbol-plist)
  (:export #:evaluation-environment)
  ;; Run-time environment accessors and readers
  ;; Operators
  (:export #:operator-status #:operator-cell
           #:compiler-macro-function #:setf-expander
           #:operator-cell-value #:operator-cell-boundp
           #:operator-cell-makunbound
           #:compiler-macro-function #:setf-expander)
  ;; Variables
  (:export #:variable-status #:variable-cell #:variable-macro-expander
           #:variable-cell-value #:variable-cell-boundp
           #:variable-cell-makunbound
           #:symbol-plist)
  ;; Types and classes
  (:export #:type-cell #:type-expander #:type-cell-value #:type-cell-boundp
           #:type-cell-makunbound)
  ;; Packages
  (:shadow #:find-package)
  (:export #:find-package #:map-all-packages)
  ;; Proclamations
  (:export #:proclamation)
  ;; Compilation environment accessors
  (:export #:function-description #:variable-description
           #:type-description #:optimize-description))

;;; High level API.
(defpackage #:clostrum
  (:use #:cl)
  ;; for reexport
  (:shadowing-import-from #:clostrum-sys
                          #:find-package #:compiler-macro-function
                          #:symbol-plist)
  (:import-from #:clostrum-sys #:type-expander
                #:function-description #:variable-description
                #:type-description #:optimize-description #:proclamation
                #:evaluation-environment #:map-all-packages)
  ;; Protocol classes:
  (:export #:run-time-environment #:compilation-environment)
  ;; Protocol functions:
  (:export #:evaluation-environment)
  ;; Operators
  (:shadow #:fdefinition #:fboundp #:fmakunbound #:macro-function
           #:special-operator-p)
  (:export #:fdefinition #:fboundp #:fmakunbound #:macro-function
           #:special-operator-p #:compiler-macro-function)
  (:export #:setf-expander #:make-special-operator)
  ;; Variables
  (:shadow #:symbol-value #:boundp #:makunbound)
  (:export #:symbol-value #:boundp #:makunbound)
  (:export #:make-variable #:make-parameter #:make-constant
           #:make-symbol-macro)
  (:export #:symbol-plist)
  ;; Types and classes
  (:shadow #:find-class)
  (:export #:find-class)
  (:export #:make-type #:type-expand-1 #:type-expand #:type-expander)
  ;; Packages
  (:export #:find-package #:map-all-packages)
  ;; Proclamations
  (:export #:proclamation)
  ;; General
  (:shadow #:macroexpand-1 #:macroexpand #:constantp)
  (:export #:macroexpand-1 #:macroexpand #:constantp)
  ;; Compilation environment
  (:export #:function-description #:variable-description
           #:type-description #:optimize-description)
  ;; Condition types:
  (:export #:attempt-to-set-constant-value
           #:attempt-to-define-special-variable-for-existing-constant
           #:attempt-to-define-special-variable-for-existing-symbol-macro
           #:attempt-to-redefine-constant-incompatibly
           #:attempt-to-define-constant-for-existing-special-variable
           #:attempt-to-define-constant-for-existing-symbol-macro
           #:attempt-to-define-symbol-macro-for-existing-special-variable
           #:attempt-to-define-symbol-macro-for-existing-constant
           #:attempt-to-define-a-setf-expander-of-non-existing-function-or-macro
           #:undefined-class))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:sys #:clostrum-sys)
                    (#:env #:clostrum)))
