(cl:in-package #:clostrum-trucler)

(defmethod trucler:describe-variable
    (client (environment env:environment) name)
  (ecase (env:variable-status client environment name)
    ((nil) nil)
    ((:special)
     (make-instance 'trucler:global-special-variable-description
       :type (env:variable-type client environment name)
       :name name))
    ((:constant)
     (make-instance 'trucler:constant-variable-description
       :name name
       :value (env:symbol-value client environment name)))
    ((:symbol-macro)
     (make-instance 'trucler:global-symbol-macro-description
       :name name
       :type (env:variable-type client environment name)
       :expansion (env:macroexpand-1 client environment name)))))

(defmethod trucler:describe-function
    (client (environment env:environment) name)
  (ecase (env:operator-status client environment name)
    ((nil) nil)
    ((:function)
     (make-instance 'trucler:global-function-description
       :name name
       :type (env:operator-ftype client environment name)
       :inline (env:operator-inline client environment name)
       :inline-data (env:operator-inline-data client environment name)
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:macro)
     (make-instance 'trucler:global-macro-description
       :name name
       :expander (env:macro-function client environment name)
       :inline (env:operator-inline client environment name)
       :compiler-macro (env:compiler-macro-function client environment name)))
    ((:special-operator)
     (make-instance 'trucler:special-operator-description
       :name name))))

(defmethod trucler:describe-optimize (client (environment env:environment))
  ;; Assume it's a possibly not normalized list.
  (let ((optimize (env:optimize client environment)))
    (flet ((quality (quality)
             (cond ((member quality optimize) 3)
                   ((assoc quality optimize) (second (assoc quality optimize)))
                   ;; FIXME: No good default.
                   (t 3))))
      (make-instance 'trucler:optimize-description
        :speed (quality 'speed) :debug (quality 'debug) :space (quality 'debug)
        :safety (quality 'safety) :compilation-speed (quality 'compilation-speed)))))

(defmethod trucler:describe-block
    (client (environment env:environment) name)
  (declare (ignore client name))
  nil)

(defmethod trucler:describe-tag
    (client (environment env:environment) tag)
  (declare (ignore client tag))
  nil)

(defmethod trucler:global-environment
    (client (environment env:environment))
  (declare (ignore client))
  environment)
