(cl:in-package #:clostrum/simple)

(deftype function-name ()
  `(or symbol (cons (eql setf) (cons symbol null))))

(deftype variable-name ()
  `symbol)

(deftype class-name ()
  `symbol)

(deftype classoid ()
  `(or class null))

(deftype package-name ()
  `string)

(deftype optimize-quality ()
  `(or symbol
       (cons symbol
             (cons (integer 0 3) null))))

(defconstant +unbound+ 'unbound)

;;; This is an unnecessary micro-optimization, yes. However having EQUAL hash
;;; tables for sake of a single irregularity (that is #'(SETF FOO) functions)
;;; is such a waste from aesthetical point of view... -- jd 2020-08-03

(defun make-storage (key-type)
  (ecase key-type
    ((symbol variable-name class-name)
     (make-hash-table :test 'eq))
    (package-name
     (make-hash-table :test 'equal))
    (function-name
     (cons (make-hash-table :test 'eq)
           (make-hash-table :test 'eq)))))

(defun access-storage (key ht)
  (cond ((atom ht)
         (values key ht))
        ((atom key)
         (values key (car ht)))
        (t
         (values (second key) (cdr ht)))))

(defun access (key ht)
  (multiple-value-bind (key ht)
      (access-storage key ht)
    (gethash key ht)))

(defun update (value key ht)
  (multiple-value-bind (key ht)
      (access-storage key ht)
    (setf (gethash key ht) value)))

(defun unbound (key ht)
  (multiple-value-bind (key ht)
      (access-storage key ht)
    (remhash key ht))
  nil)

(defmacro reference (key ht &optional default)
  (alx:with-gensyms (key* ht* value foundp)
    `(multiple-value-bind (,key* ,ht*)
         (access-storage ,key ,ht)
       (multiple-value-bind (,value ,foundp)
           (gethash ,key* ,ht*)
         (if ,foundp
             ,value
             ,default)))))

(defmacro ensure (key ht &optional default)
  (alx:with-gensyms (key* ht*)
    `(multiple-value-bind (,key* ,ht*)
         (access-storage ,key ,ht)
       (alx:ensure-gethash ,key* ,ht* ,default))))

;;; Dummy client (for the specialization).
(defclass simple-client () ())

;;; RS is not gonna like it. -- jd 2020-08-03
(defmacro define-class (name supers (&rest slot-names) &rest options)
  `(defclass ,name ,supers
     ,(mapcar (lambda (name)
                (destructuring-bind (name &optional key-type)
                    (alx:ensure-list name)
                  (let ((initarg (alx:make-keyword name))
                        (initform (if key-type
                                      `(:initform (make-storage ',key-type))
                                      nil)))
                    `(,name :initarg ,initarg :reader ,name ,@initform))))
       slot-names)
     ,@options))


;;; Run-time environment

;;; Here we take a naive approach where each operator and variable type have a
;;; different storage. Better strategy would be to have a separate cell which
;;; contains all information about the object in its namespace.
(define-class simple-run-time-environment (env:run-time-environment)
  (;; Operators
   (special-operators function-name)
   (functions function-name)
   (macro-functions symbol)
   (compiler-macro-functions function-name)
   (function-types function-name)
   (function-inlines function-name)
   ;; Variables
   (constants symbol)
   (specials symbol)
   (symbol-macros symbol)
   (variable-types symbol)
   ;; Other
   (classes symbol)
   (setf-expanders symbol)
   (type-expanders symbol)
   (packages package-name)
   (declarations symbol)))


;;; Functions

(defmethod env:fboundp
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (if (or (access function-name (functions env))
          (access function-name (special-operators env))
          (access function-name (macro-functions env)))
      t
      nil))

(defmethod env:fmakunbound
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (unbound function-name (functions env))
  (unbound function-name (special-operators env))
  (unbound function-name (macro-functions env))
  (unbound function-name (setf-expanders env))
  t)

(defmethod env:special-operator
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (access function-name (special-operators env)))

(defmethod (setf env:special-operator)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     function-name)
  (if (null new-value)
      (unbound function-name (special-operators env))
      (cond
        ((access function-name (functions env))
         (error 'env:attempt-to-define-special-operator-for-existing-function
                :function-name function-name))
        ((access function-name (macro-functions env))
         (error 'env:attempt-to-define-special-operator-for-existing-macro
                :function-name function-name))
        (t
         (update new-value function-name (special-operators env))))))

(defmethod env:fdefinition
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (cond ((alx:when-let ((def (access function-name (functions env))))
           (values def 'cl:function)))
        ((alx:when-let ((def (access function-name (macro-functions env))))
           (values def 'cl:macro-function)))
        ((alx:when-let ((def (access function-name (special-operators env))))
           (values def 'cl:special)))
        (t
         (let ((def (env:function-unbound client env function-name)))
           (values def 'cl:undefined-function)))))

(defmethod (setf env:fdefinition)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     function-name)
  (when (access function-name (special-operators env))
    (error 'env:attempt-to-define-function-for-existing-special-operator
           :function-name function-name))
  (unbound function-name (macro-functions env))
  (update new-value function-name (functions env)))

(defmethod env:macro-function
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (access symbol (macro-functions env)))

(defmethod (setf env:macro-function)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  ;; The special operator is not modified.
  (unbound symbol (functions env))
  (if (null new-value)
      (unbound symbol (macro-functions env))
      (update new-value symbol (macro-functions env))))

(defmethod env:compiler-macro-function
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (access function-name (compiler-macro-functions env)))

(defmethod (setf env:compiler-macro-function)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     function-name)
  (if (null new-value)
      (unbound function-name (compiler-macro-functions env))
      (update new-value function-name (compiler-macro-functions env))))

(defmethod env:function-type
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (or (access function-name (function-types env))
      (not (null (access function-name (functions env))))))

(defmethod (setf env:function-type)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     function-name)
  (cond
    ((access function-name (special-operators env))
     (error 'env:attempt-to-set-function-type-of-special-operator
            :function-name function-name))
    ((access function-name (macro-functions env))
     (error 'env:attempt-to-set-function-type-of-macro
            :function-name function-name))
    (t
     (update new-value function-name (function-types env)))))

(defmethod env:function-inline
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (if (access function-name (functions env))
      (values (access function-name (function-inlines env)))
      nil))

(defmethod (setf env:function-inline)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     function-name)
  (if (access function-name (functions env))
      (update new-value function-name (function-inlines env))
      (error 'env:attempt-to-declare-inline-a-non-existing-function
             :function-name function-name)))

(defmethod env:function-unbound
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  (lambda (&rest args)
    (declare (ignore args))
    (error 'undefined-function :name function-name)))

(defmethod env:function-description
    ((client simple-client)
     (env simple-run-time-environment)
     function-name)
  nil)


;;; Variables

(defmethod env:boundp
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (if (or (nth-value 1 (access symbol (constants env)))
          (nth-value 1 (access symbol (specials env)))
          ;; Symbol macro is not a variable.
          #+ (or) (nth-value 1 (access symbol (symbol-macros env))))
      t
      nil))

(defmethod env:constant-variable
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (values foundp value)))

(defmethod (setf env:constant-variable)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (if foundp
        (if (not (eql value new-value))
            (error 'env:attempt-to-define-constant-for-existing-constant
                   :name symbol)
            value)
        (cond
          ((nth-value 1 (access symbol (specials env)))
            (error 'env:attempt-to-define-constant-for-existing-special-variable
                   :name symbol))
          ((nth-value 1 (access symbol (symbol-macros env)))
           (error 'env:attempt-to-define-constant-for-existing-symbol-macro
                   :name symbol))
          (t
           (update new-value symbol (constants env)))))))

(defmethod env:special-variable
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (multiple-value-bind (value foundp)
      (access symbol (specials env))
    (values foundp value)))

(defmethod (setf env:special-variable)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol
     init-p)
  (cond
    ((nth-value 1 (access symbol (constants env)))
     (error 'env:attempt-to-define-special-variable-for-existing-constant
            :name symbol))
    ((nth-value 1 (access symbol (symbol-macros env)))
     (error 'env:attempt-to-define-special-variable-for-existing-symbol-macro
            :name symbol))
    (t
     (if init-p
         (update new-value symbol (specials env))
         (ensure symbol (specials env) +unbound+)))))

(defmethod env:symbol-macro
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (alx:if-let ((def (access symbol (symbol-macros env))))
    (values def (funcall def symbol env))
    (values nil nil)))

(defmethod (setf env:symbol-macro)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (cond
    ((nth-value 1 (access symbol (constants env)))
     (error 'env:attempt-to-define-symbol-macro-for-existing-constant
            :name symbol))
    ((nth-value 1 (access symbol (specials env)))
     (error 'env:attempt-to-define-symbol-macro-for-existing-special-variable
            :name symbol))
    (t
     (update (constantly new-value) symbol (symbol-macros env)))))

(defmethod env:variable-type
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (if foundp
        (type-of value)
        (or (access symbol (variable-types env))
            t))))

(defmethod (setf env:variable-type)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (if (nth-value 1 (access symbol (constants env)))
      (error "Can't proclaim a type of a constant ~s." symbol)
      (update new-value symbol (variable-types env))))

(defmethod env:variable-unbound
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  +unbound+)

(defmethod env:variable-description
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  nil)


;;; Other

(defmethod env:find-class
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (values (access symbol (classes env))))

(defmethod (setf env:find-class)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (if (null new-value)
      (unbound symbol (classes env))
      (update new-value symbol (classes env))))

(defmethod env:class-description
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  nil)

(defmethod env:setf-expander
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (values (access symbol (setf-expanders env))))

(defmethod (setf env:setf-expander)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (cond ((null new-value)
         (unbound symbol (setf-expanders env)))
        ((or (access symbol (functions env))
             (access symbol (macro-functions env)))
         (update new-value symbol (classes env)))
        (t
         (error "~s is not a function nor a macro." symbol))))

(defmethod env:type-expander
    ((client simple-client)
     (env simple-run-time-environment)
     symbol)
  (values (access symbol (type-expanders env))))

(defmethod (setf env:type-expander)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     symbol)
  (if (null new-value)
      (unbound symbol (type-expanders env))
      (update new-value symbol (type-expanders env))))

(defmethod env:find-package
    ((client simple-client)
     (env simple-run-time-environment)
     name)
  (values (access name (packages env))))

(defmethod (setf env:find-package)
    (new-package
     (client simple-client)
     (env simple-run-time-environment)
     name)
  (if (null new-package)
      (unbound name (packages env))
      (update new-package name (packages env))))


;;; Proclamations

(defmethod env:proclamation
    ((client simple-client)
     (env simple-run-time-environment)
     name)
  (access name (declarations env)))

(defmethod (setf env:proclamation)
    (new-value
     (client simple-client)
     (env simple-run-time-environment)
     name)
  (cond ((null new-value)
         (unbound name (declarations env)))
        (t 
         (update new-value name (declarations env)))))

;;; Compilation environment

(define-class simple-compilation-environment (env:compilation-environment)
  ((function-descriptions function-name)
   (variable-descriptions variable-name)
   (class-descriptions class-name)))

(defmethod env:function-description
    ((client simple-client)
     (env simple-compilation-environment)
     function-name)
  (or (access function-name (function-descriptions env))
      (env:function-description client (env:parent env) function-name)))

(defmethod (setf function-description)
    (description
     (client simple-client)
     (env simple-compilation-environment)
     function-name)
  (update description function-name (function-descriptions env)))

(defmethod variable-description
    ((client simple-client)
     (env simple-compilation-environment)
     symbol)
  (or (access symbol (variable-descriptions env))
      (env:variable-description client (env:parent env) symbol)))

(defmethod (setf variable-description)
    (description
     (client simple-client)
     (env simple-compilation-environment)
     symbol)
  (update description symbol (variable-descriptions env)))

(defmethod class-description
    ((client simple-client)
     (env simple-compilation-environment)
     symbol)
  (or (access symbol (class-descriptions env))
      (env:class-description client (env:parent env) symbol)))

(defmethod (setf class-description)
    (description
     (client simple-client)
     (env simple-compilation-environment)
     symbol)
  (update description symbol (class-descriptions env)))
