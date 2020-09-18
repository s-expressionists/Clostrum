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
  (:shadow #:class-name #:package-name
           #:macro-function #:compiler-macro-function)
  (:export #:virtual-client
           #:virtual-run-time-environment
           #:virtual-evaluation-environment
           #:virtual-compilation-environment)
  (:export #:function-cell
           #:variable-cell))
(in-package #:clostrum/virtual)

;;; Cell functions are _not_ part of the clostrum protocol.

;; A call to this function always succeeds.  It returns a CONS cell, in which
;; the CAR always holds the current definition of the function named
;; FUNCTION-NAME. When FUNCTION-NAME has no definition as a function, the CAR
;; of this cell will contain a function that, when called, signals an error of
;; type UNDEFINED-FUNCTION. This object is the return value of the function
;; FUNCTION-UNBOUND. The return value of this function is always the same (in
;; the sense of EQ) when it is passed the same (in the sense of EQUAL)
;; function name and the same (in the sense of EQ) environment.
(defgeneric function-cell (client environment function-name))

;; A call to this function always succeeds. It returns a CONS cell, in which
;; the CAR always holds the current definition of the variable named SYMBOL.
;; When SYMBOL has no definition as a variable, the CAR of this cell will
;; contain an object that indicates that the variable is unbound. This object
;; is the return value of the function VARIABLE-UNBOUND. The return value of
;; this function is always the same (in the sense of EQ) when it is passed the
;; same symbol and the same environment.
(defgeneric variable-cell (client environment symbol))


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

(defclass function-entry ()
  ((name
    :initarg :name
    :reader name)
   ;; CAR contains the function, CDR contains unbound function substitute
   ;; which signals the error.
   (cell
    :reader cell)
   (special-operator
    :initform nil
    :accessor special-operator)
   (macro-function
    :initform nil
    :accessor macro-function)
   (compiler-macro-function
    :initform nil
    :accessor compiler-macro-function)
   (function-type
    :initform nil
    :accessor function-type)
   (function-inline
    :initform nil
    :accessor function-inline)
   (setf-expander
    :initform nil
    :accessor setf-expander))
  (:default-initargs :name (error "The initarg :NAME is required.")))

(defmethod initialize-instance :after ((instance function-entry) &key name)
  (let ((funb (lambda (&rest args)
                (declare (ignore args))
                (error 'undefined-function :name name))))
    (setf (slot-value instance 'cell)
          (cons funb funb))))

(defclass variable-entry ()
  ((name
    :initarg :name
    :reader name)
   ;; CAR contains the value, CDR contains the unbound marker. CDR is
   ;; redundant and not used in the code.
   (cell
    :reader cell
    :initform (cons +unbound+ +unbound+))
   (constant-variable
    :initform nil
    :accessor constant-variable)
   (special-variable
    :initform nil
    :accessor special-variable)
   (symbol-macro
    :initform nil
    :accessor symbol-macro)
   (variable-type
    :initform nil
    :accessor variable-type)
   (type-expander
    :initform nil
    :accessor type-expander))
  (:default-initargs :name (error "The initarg :NAME is required.")))

;;; Here we take a naive approach where each operator and variable type have a
;;; different storage. Better strategy would be to have a separate cell which
;;; contains all information about the object in its namespace.
(defclass virtual-run-time-environment (env:run-time-environment)
  ((functions
    :reader functions
    :initform (make-hash-table :test #'equal))
   (variables
    :reader variables
    :initform (make-hash-table :test #'eq))
   (classes
    :reader classes
    :initform (make-hash-table :test #'eq))
   (packages
    :reader packages
    :initform (make-hash-table :test #'equal))
   (declarations
    :initarg :declarations
    :reader declarations
    :initform (make-storage 'symbol))))

(defun get-function-entry (function-name env)
  (alx:ensure-gethash function-name (functions env)
                      (make-instance 'function-entry :name function-name)))

(defun function-bound-p (function-entry)
  (let ((cell (cell function-entry)))
    (not (eq (car cell) (cdr cell)))))

(defun get-variable-entry (variable-name env)
  (alx:ensure-gethash variable-name (variables env)
                      (make-instance 'variable-entry :name variable-name)))

(defun variable-bound-p (variable-entry)
  (let ((cell (cell variable-entry)))
    (not (eq (car cell) +unbound+))))


;;; Functions
(defmethod function-cell ((client virtual-client)
                          (env virtual-run-time-environment)
                          function-name)
  (check-type function-name function-name)
  (cell (get-function-entry function-name env)))

(defmethod env:fboundp
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (if (or (function-bound-p entry)
            (special-operator entry)
            (macro-function entry))
        t
        nil)))

(defmethod env:fmakunbound
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let* ((entry (get-function-entry function-name env))
         (cell (cell entry)))
    (setf (car cell) (cdr cell))
    (setf (special-operator entry) nil)
    (setf (macro-function entry) nil)
    (setf (setf-expander entry) nil))
  t)

(defmethod env:special-operator
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (special-operator (get-function-entry function-name env)))

(defmethod (setf env:special-operator)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (if (null new-value)
        (setf (special-operator entry) nil)
        (cond
          ((function-bound-p entry)
           (error "~s already names a function." function-name))
          ((macro-function entry)
           (error "~s already names a macro." function-name))
          (t
           (setf (special-operator entry) new-value))))))

(defmethod env:fdefinition
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let* ((entry (get-function-entry function-name env))
         (cell (cell entry)))
    (cond ((function-bound-p entry)
           (values (car cell) 'cl:function))
          ((alx:when-let ((def (macro-function entry)))
             (values def 'cl:macro-function)))
          ((alx:when-let ((def (macro-function entry)))
             (values def 'cl:special)))
          (t
           (values (cdr cell) 'cl:undefined-function)))))

(defmethod (setf env:fdefinition)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (check-type new-value function)
  (let* ((entry (get-function-entry function-name env))
         (cell (cell entry)))
    (when (special-operator entry)
      (error "~s already names a special operator." function-name))
    (setf (macro-function entry) nil)
    (setf (car cell) (or new-value (cdr cell)))))

(defmethod env:macro-function
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (macro-function (get-function-entry symbol env)))

(defmethod (setf env:macro-function)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (check-type new-value (or function null))
  (let* ((entry (get-function-entry symbol env))
         (cell (cell entry)))
    ;; The special operator is not modified.
    (setf (car cell) (cdr cell))
    (setf (macro-function entry) new-value)))

(defmethod env:compiler-macro-function
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (compiler-macro-function (get-function-entry function-name env)))

(defmethod (setf env:compiler-macro-function)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (setf (compiler-macro-function entry) new-value)))

(defmethod env:function-type
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (or (function-type entry)
        (function-bound-p entry))))

(defmethod (setf env:function-type)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (cond
      ((special-operator entry)
       (error "~s can't be a special operator." function-name))
      ((macro-function entry)
       (error "~s can't be a macro." function-name))
      (t
       (setf (function-type entry) new-value)))))


(defmethod env:function-inline
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (let ((entry (get-function-entry function-name env)))
    (and (function-bound-p entry)
         (function-inline entry))))

(defmethod (setf env:function-inline)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (check-type new-value (member nil cl:inline cl:notinline))
  (let ((entry (get-function-entry function-name env)))
    (if (function-bound-p entry)
        (setf (function-inline entry) new-value)
        (error "The function ~s doesn't exist." function-name))))

(defmethod env:function-unbound
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  (cdr (cell (get-function-entry function-name env))))

(defmethod env:function-description
    ((client virtual-client)
     (env virtual-run-time-environment)
     function-name)
  (check-type function-name function-name)
  nil)

(defmethod env:setf-expander
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (setf-expander (get-function-entry symbol env)))

(defmethod (setf env:setf-expander)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-function-entry symbol env)))
    (cond ((or (null new-value)
               (function-bound-p entry)
               (macro-function entry))
           (setf (setf-expander entry) new-value))
          (t
           (error "~s is not a function nor a macro." symbol)))))


;;; Variables
(defmethod variable-cell ((client virtual-client)
                          (env virtual-run-time-environment)
                          symbol)
  (check-type symbol symbol)
  (cell (get-variable-entry symbol env)))

(defmethod env:boundp
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  ;; SYMBOL-MACRO has a value in the variable cell, however it is not treated
  ;; as a bound variable (following behavior of other implementations). It is
  ;; not clearly defined what does bound mean in context of the symbol-macro,
  ;; but since it is expanded it is not a variable (so can't be bound).
  (let ((entry (get-variable-entry symbol env)))
    (or (constant-variable entry)
        (special-variable entry))))

(defmethod env:constant-variable
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (values (constant-variable entry) (car (cell entry)))))

(defmethod (setf env:constant-variable)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let* ((entry (get-variable-entry symbol env))
         (cell (cell entry)))
    (if (constant-variable entry)
        (let ((value (car cell)))
          (if (not (eql value new-value))
              (error "~s is already defined as a constant." symbol)
              value))
        (cond
          ((special-variable entry)
           (error "~s is already defined as a special variable." symbol))
          ((symbol-macro entry)
           (error "~s is already defined as a symbol macro." symbol))
          (t
           (setf (constant-variable entry) t)
           (setf (car cell) new-value))))))

(defmethod env:special-variable
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (if (special-variable entry)
        (values t (car (cell entry)))
        (values nil nil))))

(defmethod (setf env:special-variable)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol
     init-p)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (cond ((constant-variable entry)
           (error "~s is already defined as a constant." symbol))
          ((symbol-macro entry)
           (error "~s is already defined as a symbol macro." symbol))
          (t
           (setf (special-variable entry) t)
           (when init-p
             (setf (car (cell entry)) new-value))))))

(defmethod env:symbol-macro
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (if (symbol-macro entry)
        (let ((def (car (cell entry))))
          (values def (funcall def symbol env)))
        (values nil nil))))

(defmethod (setf env:symbol-macro)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (cond
      ((constant-variable entry)
       (error "~s is already defined as a constant." symbol))
      ((special-variable entry)
       (error "~s is already defined as a special variable." symbol))
      (t
       (setf (symbol-macro entry) t)
       (setf (car (cell entry)) (constantly new-value))))))

(defmethod env:variable-type
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (if (constant-variable entry)
        (type-of (car (cell entry)))
        (or (variable-type entry)
            t))))

(defmethod (setf env:variable-type)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (if (constant-variable entry)
        (error "Can't proclaim a type of a constant ~s." symbol)
        (setf (variable-type entry) new-value))))

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

(defmethod env:type-expander
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (type-expander entry)))

(defmethod (setf env:type-expander)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (let ((entry (get-variable-entry symbol env)))
    (setf (type-expander entry) new-value)))


;;; Other

(defmethod env:find-class
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  (values (gethash symbol (classes env))))

(defmethod (setf env:find-class)
    (new-value
     (client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  #+ (or) (check-type new-value classoid)
  (if (null new-value)
      (remhash symbol (classes env))
      (setf (gethash symbol (classes env))
            new-value)))

(defmethod env:class-description
    ((client virtual-client)
     (env virtual-run-time-environment)
     symbol)
  (check-type symbol symbol)
  nil)

(defmethod env:find-package
    ((client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name package-name)
  (values (gethash name (packages env))))

(defmethod (setf env:find-package)
    (new-package
     (client virtual-client)
     (env virtual-run-time-environment)
     name)
  (check-type name package-name)
  (check-type new-package (or null package))
  (if (null new-package)
      (remhash name (packages env))
      (setf (gethash name (packages env)) new-package)))


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
