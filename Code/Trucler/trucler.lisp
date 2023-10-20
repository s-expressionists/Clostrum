(cl:in-package #:clostrum-trucler)

(defmethod env:variable-description
    (client (environment env:run-time-environment) name)
  (ecase (sys:variable-status client environment name)
    ((nil) nil)
    ((:special)
     (make-instance 'trucler:global-special-variable-description
       :name name))
    ((:constant)
     (make-instance 'trucler:constant-variable-description
       :name name
       :value (env:symbol-value client environment name)))
    ((:symbol-macro)
     (make-instance 'trucler:global-symbol-macro-description
       :name name
       :expansion (env:macroexpand-1 client environment name)))))

(defmethod trucler:describe-variable
    (client (environment env:run-time-environment) name)
  (env:variable-description client environment name))

(defmethod trucler:describe-variable
    (client (environment env:compilation-environment) name)
  (or (env:variable-description client environment name)
      (env:variable-description
       client (env:evaluation-environment client environment) name)))

(defmethod env:function-description
    (client (environment env:run-time-environment) name)
  (ecase (sys:operator-status client environment name)
    ((nil) nil)
    ((:function)
     (make-instance 'trucler:global-function-description
       :name name
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:macro)
     (make-instance 'trucler:global-macro-description
       :name name
       :expander (env:macro-function client environment name)
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:special-operator)
     (make-instance 'trucler:special-operator-description
       :name name))))

(defmethod trucler:describe-function
    (client (environment env:run-time-environment) name)
  (env:function-description client environment name))

(defmethod trucler:describe-function
    (client (environment env:compilation-environment) name)
  (or (env:function-description client environment name)
      (env:function-description
       client (env:evaluation-environment client environment) name)))

(defmethod trucler:describe-optimize
    (client (environment env:compilation-environment))
  (env:optimize-description client environment))

(defmethod trucler:describe-block
    (client (environment env:compilation-environment) name)
  (declare (ignore client name))
  nil)

(defmethod trucler:describe-tag
    (client (environment env:compilation-environment) tag)
  (declare (ignore client name))
  nil)

(defmethod trucler:global-environment
    (client (environment env:run-time-environment))
  (declare (ignore client))
  environment)

(defmethod trucler:global-environment
    (client (environment env:compilation-environment))
  (declare (ignore client))
  environment)
