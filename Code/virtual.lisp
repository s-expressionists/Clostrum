;;; This file contains an example implementation of the clostrum protocol
;;; using hash tables. It is not tied to any implementation internals.

;;; TODO
;;;
;;; - add entries like function-inline in the compilation environment
;;; - specify error conditions in documentation and use them

(defpackage #:clostrum/virtual
  (:use #:cl)
  (:local-nicknames (#:env #:clostrum)
                    (#:alx #:alexandria))
  (:shadow #:class-name #:package-name)
  (:export #:virtual-client
           #:virtual-run-time-environment
           #:virtual-evaluation-environment
           #:virtual-compilation-environment))
(in-package #:clostrum/virtual)

(deftype function-name ()
  `(or symbol (cons (eql setf) (cons symbol null))))

(deftype variable-name ()
  `symbol)

(deftype class-name ()
  `symbol)

;;; KLUDGE SICL likes to assign anything to the class during bootstrap, so it
;;; is defined as T.
(deftype classoid ()
  t)

#+ (or)
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
(defclass virtual-client () ())


;;; Run-time environment

;;; Here we take a naive approach where each operator and variable type have a
;;; different storage. Better strategy would be to have a separate cell which
;;; contains all information about the object in its namespace.
(defclass virtual-run-time-environment (env:run-time-environment)
  ((special-operators
    :initarg :special-operators
    :reader special-operators
    :initform (make-storage 'function-name))
   (functions
    :initarg :functions
    :reader functions
    :initform (make-storage 'function-name))
   (macro-functions
    :initarg :macro-functions
    :reader macro-functions
    :initform (make-storage 'symbol))
   (compiler-macro-functions
    :initarg :compiler-macro-functions
    :reader compiler-macro-functions
    :initform (make-storage 'function-name))
   (function-types
    :initarg :function-types
    :reader function-types
    :initform (make-storage 'function-name))
   (function-inlines
    :initarg :function-inlines
    :reader function-inlines
    :initform (make-storage 'function-name))
   (constants
    :initarg :constants
    :reader constants
    :initform (make-storage 'symbol))
   (specials
    :initarg :specials
    :reader specials
    :initform (make-storage 'symbol))
   (symbol-macros
    :initarg :symbol-macros
    :reader symbol-macros
    :initform (make-storage 'symbol))
   (variable-types
    :initarg :variable-types
    :reader variable-types
    :initform (make-storage 'symbol))
   (classes
    :initarg :classes
    :reader classes
    :initform (make-storage 'symbol))
   (setf-expanders
    :initarg :setf-expanders
    :reader setf-expanders
    :initform (make-storage 'symbol))
   (type-expanders
    :initarg :type-expanders
    :reader type-expanders
    :initform (make-storage 'symbol))
   (packages
    :initarg :packages
    :reader packages :initform
    (make-storage 'package-name))
   (declarations
    :initarg :declarations
    :reader declarations
    :initform (make-storage 'symbol))))


;;; Functions

(defmethod env:fboundp
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (if (or (access function-name (functions env))
          (access function-name (special-operators env))
          (access function-name (macro-functions env)))
      t
      nil))

(defmethod env:fmakunbound
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (unbound function-name (functions env))
  (unbound function-name (special-operators env))
  (unbound function-name (macro-functions env))
  (unbound function-name (setf-expanders env))
  t)

(defmethod env:special-operator
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (access function-name (special-operators env)))

(defmethod (setf env:special-operator)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (if (null new-value)
      (unbound function-name (special-operators env))
      (cond
        ((access function-name (functions env))
         (error "~s already names a function." function-name))
        ((access function-name (macro-functions env))
         (error "~s already names a macro." function-name))
        (t
         (update new-value function-name (special-operators env))))))

(defmethod env:fdefinition
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
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
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (check-type new-value function)
  (when (access function-name (special-operators env))
    (error "~s already names a special operator." function-name))
  (unbound function-name (macro-functions env))
  (update new-value function-name (functions env)))

(defmethod env:macro-function
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (access symbol (macro-functions env)))

(defmethod (setf env:macro-function)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (check-type new-value (or function null))
  ;; The special operator is not modified.
  (unbound symbol (functions env))
  (if (null new-value)
      (unbound symbol (macro-functions env))
      (update new-value symbol (macro-functions env))))

(defmethod env:compiler-macro-function
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (access function-name (compiler-macro-functions env)))

(defmethod (setf env:compiler-macro-function)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (if (null new-value)
      (unbound function-name (compiler-macro-functions env))
      (update new-value function-name (compiler-macro-functions env))))

(defmethod env:function-type
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (or (access function-name (function-types env))
      (not (null (access function-name (functions env))))))

(defmethod (setf env:function-type)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (cond
    ((access function-name (special-operators env))
     (error "~s can't be a special operator." function-name))
    ((access function-name (macro-functions env))
     (error "~s can't be a macro." function-name))
    (t
     (update new-value function-name (function-types env)))))


(defmethod env:function-inline
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (if (access function-name (functions env))
      (values (access function-name (function-inlines env)))
      nil))

(defmethod (setf env:function-inline)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (check-type new-value (member nil cl:inline cl:notinline))
  (if (access function-name (functions env))
      (update new-value function-name (function-inlines env))
      (error "The function ~s doesn't exist." function-name)))

(defmethod env:function-unbound
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (lambda (&rest args)
    (declare (ignore args))
    (error 'undefined-function :name function-name)))

(defmethod env:function-description
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  nil)


;;; Variables

(defmethod env:boundp
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (if (or (nth-value 1 (access symbol (constants env)))
          (nth-value 1 (access symbol (specials env)))
          (nth-value 1 (access symbol (symbol-macros env))))
      t
      nil))

(defmethod env:constant-variable
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (values foundp value)))

(defmethod (setf env:constant-variable)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (if foundp
        (if (not (eql value new-value))
            (error "~s is already defined as a constant." symbol)
            value)
        (cond
          ((nth-value 1 (access symbol (specials env)))
           (error "~s is already defined as a special variable." symbol))
          ((nth-value 1 (access symbol (symbol-macros env)))
           (error "~s is already defined as a symbol macro." symbol))
          (t
           (update new-value symbol (constants env)))))))

(defmethod env:special-variable
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (multiple-value-bind (value foundp)
      (access symbol (specials env))
    (values foundp value)))

(defmethod (setf env:special-variable)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol
     init-p)
  (check-type symbol symbol)
  (cond
    ((nth-value 1 (access symbol (constants env)))
     (error "~s is already defined as a constant." symbol))
    ((nth-value 1 (access symbol (symbol-macros env)))
     (error "~s is already defined as a symbol macro." symbol))
    (t
     (if init-p
         (update new-value symbol (specials env))
         (ensure symbol (specials env) +unbound+)))))

(defmethod env:symbol-macro
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (alx:if-let ((def (access symbol (symbol-macros env))))
    (values def (funcall def symbol env))
    (values nil nil)))

(defmethod (setf env:symbol-macro)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (cond
    ((nth-value 1 (access symbol (constants env)))
     (error "~s is already defined as a constant." symbol))
    ((nth-value 1 (access symbol (specials env)))
     (error "~s is already defined as a special variable." symbol))
    (t
     (update (constantly new-value) symbol (symbol-macros env)))))

(defmethod env:variable-type
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (multiple-value-bind (value foundp)
      (access symbol (constants env))
    (if foundp
        (type-of value)
        (or (access symbol (variable-types env))
            t))))

(defmethod (setf env:variable-type)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (if (nth-value 1 (access symbol (constants env)))
      (error "Can't proclaim a type of a constant ~s." symbol)
      (update new-value symbol (variable-types env))))

(defmethod env:variable-unbound
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  +unbound+)

(defmethod env:variable-description
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  nil)


;;; Other

(defmethod env:find-class
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (values (access symbol (classes env))))

(defmethod (setf env:find-class)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  ;;(check-type new-value classoid)
  (if (null new-value)
      (unbound symbol (classes env))
      (update new-value symbol (classes env))))

(defmethod env:class-description
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  nil)

(defmethod env:setf-expander
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (values (access symbol (setf-expanders env))))

(defmethod (setf env:setf-expander)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (cond ((null new-value)
         (unbound symbol (setf-expanders env)))
        ((or (access symbol (functions env))
             (access symbol (macro-functions env)))
         (update new-value symbol (classes env)))
        (t
         (error "~s is not a function nor a macro." symbol))))

(defmethod env:type-expander
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (values (access symbol (type-expanders env))))

(defmethod (setf env:type-expander)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (if (null new-value)
      (unbound symbol (type-expanders env))
      (update new-value symbol (type-expanders env))))

(defmethod env:find-package
    ((client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name package-name)
  (values (access name (packages env))))

(defmethod (setf env:find-package)
    (new-package
     (client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name package-name)
  (check-type new-package (or null package))
  (if (null new-package)
      (unbound name (packages env))
      (update new-package name (packages env))))


;;; Declarations

(defmethod env:find-declaration
    ((client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name symbol)
  (access name (declarations env)))

(defmethod (setf env:find-declaration)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name symbol)
  (cond ((null new-value)
         (unbound name (declarations env)))
        ((member name (access 'cl:declaration (declarations env)))
         (update new-value name (declarations env)))
        (t
         (error "~s is not a known declaration name." name))))

;;; Undefined behavior: it is not specified whether new optimization qualities
;;; merge with previous ones or maybe rather replace them. This implementation
;;; merges optimization qualities, however alternative approach might be more
;;; useful in some contexts. In that case it would be enough to remove this
;;; method, a default method replaces the declaration value. -- jd 2020-08-24
(defmethod (setf env:find-declaration)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     (name (eql 'cl:optimize)))
  (check-type new-value list)
  (if (null new-value)
      ;; When the new-value is NIL then remove all qualities.
      (unbound name (declarations env))
      (loop with qualities = (access 'cl:optimize (declarations env))
            for new-quality in new-value
            do (check-type new-quality optimize-quality)
               (destructuring-bind (name &optional (value 3))
                   (alx:ensure-list new-quality)
                 (setf (getf qualities name) value))
            finally (update qualities 'cl:optimize (declarations env)))))

(defmethod (setf env:find-declaration)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     (name (eql 'cl:declaration)))
  (check-type new-value list)
  (if (null new-value)
      ;; When the new-value is NIL then remove all declarations.
      (unbound name (declarations env))
      (loop with declarations = (access name (declarations env))
            for name in new-value
            do (check-type name symbol)
               (pushnew name declarations)
            finally (update declarations 'cl:declaration (declarations env)))))


;;; Compilation environment

(defclass virtual-compilation-environment (env:compilation-environment)
  ((function-descriptions
    :initarg :function-descriptions
    :reader function-descriptions
    :initform (make-storage 'function-name))
   (variable-descriptions
    :initarg :variable-descriptions
    :reader variable-descriptions
    :initform (make-storage 'variable-name))
   (class-descriptions
    :initarg :class-descriptions
    :reader class-descriptions
    :initform (make-storage 'class-name))))

(defmethod env:function-description
    ((client virtual-client)
     (env virtual-compilation-environment)
     function-name)
  (check-type function-name function-name)
  (or (access function-name (function-descriptions env))
      (env:function-description client (env:parent env) function-name)))

(defmethod (setf function-description)
    (description
     (client virtual-client)
     (env virtual-compilation-environment)
     function-name)
  (check-type function-name function-name)
  (update description function-name (function-descriptions env)))

(defmethod variable-description
    ((client virtual-client)
     (env virtual-compilation-environment)
     symbol)
  (check-type symbol symbol)
  (or (access symbol (variable-descriptions env))
      (env:variable-description client (env:parent env) symbol)))

(defmethod (setf variable-description)
    (description
     (client virtual-client)
     (env virtual-compilation-environment)
     symbol)
  (check-type symbol symbol)
  (update description symbol (variable-descriptions env)))

(defmethod class-description
    ((client virtual-client)
     (env virtual-compilation-environment)
     symbol)
  (check-type symbol symbol)
  (or (access symbol (class-descriptions env))
      (env:class-description client (env:parent env) symbol)))

(defmethod (setf class-description)
    (description
     (client virtual-client)
     (env virtual-compilation-environment)
     symbol)
  (check-type symbol symbol)
  (update description symbol (class-descriptions env)))
