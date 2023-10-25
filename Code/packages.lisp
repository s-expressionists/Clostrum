;;; Low level API.
(defpackage #:clostrum-sys
  (:use #:cl)
  (:shadow #:compiler-macro-function #:find-package #:symbol-plist #:optimize)
  (:export #:parent)
  ;; Run-time environment accessors and readers
  ;; Operators
  (:export #:operator-status #:operator-cell #:ensure-operator-cell
           #:compiler-macro-function #:setf-expander
           #:operator-cell-value #:operator-cell-boundp
           #:operator-cell-makunbound
           #:operator-inline #:operator-inline-known-p
           #:operator-inline-data #:operator-ftype
           #:compiler-macro-function #:setf-expander)
  ;; Variables
  (:export #:variable-status #:variable-cell #:ensure-variable-cell
           #:variable-cell-value #:variable-cell-boundp
           #:variable-cell-makunbound
           #:variable-type #:variable-macro-expander
           #:symbol-plist #:symbol-plist-known-p)
  ;; Types and classes
  (:export #:type-cell #:type-expander #:type-cell-value #:type-cell-boundp
           #:ensure-type-cell #:type-cell-makunbound)
  ;; Packages
  (:shadow #:find-package #:package-name)
  (:export #:find-package #:package-name #:package-names #:map-all-packages)
  ;; Proclamations & optimize
  (:export #:proclamation #:optimize))

;;; High level API.
(defpackage #:clostrum
  (:use #:cl)
  (:import-from #:clostrum-sys
                #:parent
                #:proclamation
                #:variable-cell-boundp #:variable-cell-value #:variable-cell-makunbound
                #:operator-cell-boundp #:operator-cell-value #:operator-cell-makunbound
                #:type-cell-boundp #:type-cell-value #:type-cell-makunbound)
  ;; Protocol classes:
  (:export #:environment #:run-time-environment #:compilation-environment)
  ;; Protocol functions:
  (:export #:parent #:merge-types #:merge-optimize)
  ;; Operators
  (:shadow #:fdefinition #:fboundp #:fmakunbound #:macro-function
           #:special-operator-p #:compiler-macro-function)
  (:export #:operator-status #:ensure-operator-cell
           #:operator-cell-boundp #:operator-cell-value #:operator-cell-makunbound)
  (:export #:fdefinition #:fboundp #:fmakunbound #:macro-function
           #:special-operator-p #:compiler-macro-function
           #:operator-ftype #:operator-inline #:operator-inline-data)
  (:export #:setf-expander #:make-special-operator #:note-function)
  ;; Variables
  (:shadow #:symbol-value #:boundp #:makunbound #:symbol-plist)
  (:export #:ensure-variable-cell
           #:variable-cell-boundp #:variable-cell-value #:variable-cell-makunbound)
  (:export #:variable-status #:symbol-value #:boundp #:makunbound)
  (:export #:make-variable #:make-parameter #:make-constant
           #:make-symbol-macro #:variable-macro-expander #:variable-type)
  (:export #:symbol-plist)
  ;; Types and classes
  (:shadow #:find-class)
  (:export #:ensure-type-cell
           #:type-cell-boundp #:type-cell-value #:type-cell-makunbound)
  (:export #:find-class)
  (:export #:type-expand-1 #:type-expand #:type-expander)
  ;; Packages
  (:shadow #:find-package #:package-name)
  (:export #:find-package #:package-name #:package-names #:map-all-packages)
  ;; Proclamations & optimize
  (:shadow #:optimize)
  (:export #:proclamation #:optimize #:proclaim-optimize)
  ;; General
  (:shadow #:macroexpand-1 #:macroexpand #:constantp)
  (:export #:macroexpand-1 #:macroexpand #:constantp)
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
           #:attempt-to-set-ftype-of-non-function
           #:attempt-to-note-operator-as-function
           #:undefined-class))

(defpackage #:clostrum-implementation
  (:use #:cl)
  (:local-nicknames (#:sys #:clostrum-sys)
                    (#:env #:clostrum)))
