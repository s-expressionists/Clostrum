(cl:in-package #:clostrum-trucler)

(defmethod trucler:describe-variable
    (client (environment env:run-time-environment) name)
  (if (env:special-variable client environment name)
      (make-instance 'trucler:global-special-variable-description
        :name name
        :type (env:variable-type client environment name))
      (multiple-value-bind (constant-variable-p value)
          (env:constant-variable client environment name)
        (if constant-variable-p
            (make-instance 'trucler:constant-variable-description
              :name name
              :value value)
            (multiple-value-bind (expansion symbol-macro-p)
                (env:symbol-macro client environment name)
              (if symbol-macro-p
                  (make-instance 'trucler:global-symbol-macro-description
                    :name name
                    :type (env:variable-type client environment name)
                    :expansion expansion)
                  nil))))))

(defmethod trucler:describe-variable
    (client (environment env:compilation-environment) name)
  (trucler:describe-variable client (env:parent environment) name))

(defmethod trucler:describe-function
    (client (environment env:run-time-environment) name)
  (let ((macro-function (env:macro-function client environment name))
        (type (env:function-type client environment name)))
    (cond ((not (null macro-function))
           (make-instance 'trucler:global-macro-description
             :name name
             :expander macro-function
             :compiler-macro (env:compiler-macro-function
                              client environment name)))
          ((env:special-operator client environment name)
           (make-instance 'trucler:special-operator-description
             :name name))
          ((and (null (env:fdefinition client environment name))
                (null type))
           nil)
          (t
           (make-instance 'trucler:global-function-description
             :name name
             :type (if (null type) t type)
             :compiler-macro (env:compiler-macro-function
                              client environment name))))))

(defmethod trucler:describe-function
    (client (environment env:compilation-environment) name)
  (trucler:describe-function client (env:parent environment) name))

(defmethod trucler:describe-block
    (client (environment env:compilation-environment) name)
  nil)

(defmethod trucler:describe-tag
    (client (environment env:compilation-environment) tag)
  nil)

(defmethod trucler:global-environment
    (client (environment env:run-time-environment))
  environment)

(defmethod trucler:global-environment
    (client (environment env:compilation-environment))
  environment)
