(cl:in-package #:clostrum-basic)

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

(deftype package-name ()
  `string)

(deftype optimize-quality ()
  `(or symbol
       (cons symbol
             (cons (integer 0 3) null))))

(defconstant +unbound+ 'unbound)


;;; Function and variable entries
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



;;; Run-time environment

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
    :reader declarations
    :initform (make-hash-table :test #'eq))))

(defun get-function-entry (name env &optional createp)
  (if createp
      (alx:ensure-gethash name
                          (functions env)
                          (make-instance 'function-entry :name name))
      (gethash name (functions env) nil)))

(defun function-bound-p (function-entry)
  (let ((cell (cell function-entry)))
    (not (eq (car cell) (cdr cell)))))

(defun get-variable-entry (name env &optional createp)
  (if createp
      (alx:ensure-gethash name
                          (variables env)
                          (make-instance 'variable-entry :name name))
      (gethash name (variables env) nil)))

(defun variable-bound-p (variable-entry)
  (let ((cell (cell variable-entry)))
    (not (eq (car cell) +unbound+))))


;;; Functions
(defmethod function-cell
    (client
     (env virtual-run-time-environment)
     function-name)
  (cell (get-function-entry function-name env t)))

(defmethod env:fboundp
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:if-let ((entry (get-function-entry function-name env)))
    (and (or (function-bound-p entry)
             (special-operator entry)
             (macro-function entry))
         t)
    nil))

(defmethod env:fmakunbound
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:when-let ((entry (get-function-entry function-name env)))
    (let ((cell (cell entry)))
      (setf (car cell) (cdr cell))
      (setf (special-operator entry) nil)
      (setf (macro-function entry) nil)
      (setf (setf-expander entry) nil)))
  t)

(defmethod env:special-operator
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:if-let ((entry (get-function-entry function-name env)))
    (special-operator entry)
    nil))

(defmethod (setf env:special-operator)
    (new-value
     client
     (env virtual-run-time-environment)
     function-name)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry function-name env)))
      (setf (special-operator entry) nil))
    (return-from env:special-operator))
  (let ((entry (get-function-entry function-name env t)))
    (cond
      ((function-bound-p entry)
       (error "~s already names a function." function-name))
      ((macro-function entry)
       (error "~s already names a macro." function-name))
      (t
       (setf (special-operator entry) new-value)))))

(defmethod env:fdefinition
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:if-let ((entry (get-function-entry function-name env)))
    (cond ((function-bound-p entry)
           (values (car (cell entry)) 'cl:function))
          ((alx:when-let ((def (macro-function entry)))
             (values def 'cl:macro-function)))
          ((alx:when-let ((def (macro-function entry)))
             (values def 'cl:special)))
          (t
           (values (cdr (cell entry)) 'cl:undefined-function)))
    (values (lambda (&rest args)
              (declare (ignore args))
              (error 'undefined-function :name function-name))
            'cl:undefined-function)))

(defmethod (setf env:fdefinition)
    (new-value
     client
     (env virtual-run-time-environment)
     function-name)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry function-name env)))
      (when (special-operator entry)
        (error "~s already names a special operator." function-name))
      (setf (macro-function entry) nil)
      (let ((cell (cell entry)))
        (setf (car cell) (cdr cell))))
    (return-from env:fdefinition))
  (let ((entry (get-function-entry function-name env t)))
    (when (special-operator entry)
      (error "~s already names a special operator." function-name))
    (setf (macro-function entry) nil)
    (let ((cell (cell entry)))
      (setf (car cell) new-value))))

(defmethod env:macro-function
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:when-let ((entry (get-function-entry symbol env)))
    (macro-function entry)))

(defmethod (setf env:macro-function)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry symbol env)))
      (let ((cell (cell entry)))
        (setf (car cell) (cdr cell)))
      ;; The special operator is not modified.
      (setf (macro-function entry) nil))
    (return-from env:macro-function))
  (let* ((entry (get-function-entry symbol env t))
         (cell (cell entry)))
    (setf (car cell) (cdr cell))
    ;; The special operator is not modified.
    (setf (macro-function entry) new-value)))

(defmethod env:compiler-macro-function
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:when-let ((entry (get-function-entry function-name env)))
    (compiler-macro-function entry)))

(defmethod (setf env:compiler-macro-function)
    (new-value
     client
     (env virtual-run-time-environment)
     function-name)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry function-name env)))
      (setf (compiler-macro-function entry) nil))
    (return-from env:compiler-macro-function))
  (let ((entry (get-function-entry function-name env t)))
    (setf (compiler-macro-function entry) new-value)))

(defmethod env:function-type
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:if-let ((entry (get-function-entry function-name env)))
    (or (function-type entry)
        (function-bound-p entry))
    nil))

(defmethod (setf env:function-type)
    (new-value
     client
     (env virtual-run-time-environment)
     function-name)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry function-name env)))
      (cond
        ((special-operator entry)
         (error "~s can't be a special operator." function-name))
        ((macro-function entry)
         (error "~s can't be a macro." function-name))
        (t
         (setf (function-type entry) nil))))
    (return-from env:function-type))
  (let ((entry (get-function-entry function-name env t)))
    (cond
      ((special-operator entry)
       (error "~s can't be a special operator." function-name))
      ((macro-function entry)
       (error "~s can't be a macro." function-name))
      (t
       (setf (function-type entry) new-value)))))

(defmethod env:function-inline
    (client
     (env virtual-run-time-environment)
     function-name)
  (alx:if-let ((entry (get-function-entry function-name env)))
    (and (function-bound-p entry)
         (function-inline entry))
    nil))

(defmethod (setf env:function-inline)
    (new-value
     client
     (env virtual-run-time-environment)
     function-name)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry function-name env)))
      (if (function-bound-p entry)
          (setf (function-inline entry) nil)
          (error "The function ~s doesn't exist." function-name)))
    (return-from env:function-inline))
  (let ((entry (get-function-entry function-name env t)))
    (if (function-bound-p entry)
        (setf (function-inline entry) new-value)
        (error "The function ~s doesn't exist." function-name))))

(defmethod env:function-unbound
    (client
     (env virtual-run-time-environment)
     function-name)
  (cdr (cell (get-function-entry function-name env t))))

(defmethod env:function-description
    (client
     (env virtual-run-time-environment)
     function-name)
  nil)

(defmethod env:setf-expander
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:when-let ((entry (get-function-entry symbol env)))
    (setf-expander entry)))

(defmethod (setf env:setf-expander)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (when (null new-value)
    (alx:when-let ((entry (get-function-entry symbol env)))
      (setf (setf-expander entry) nil))
    (return-from env:setf-expander))
  (alx:if-let ((entry (get-function-entry symbol env)))
    (if (or (function-bound-p entry)
            (macro-function entry))
        (setf (setf-expander entry) new-value)
        (error "~s is not a function nor a macro." symbol))
    (error "~s is not a function nor a macro." symbol)))


;;; Variables
(defmethod variable-cell
    (client
     (env virtual-run-time-environment)
     symbol)
  (cell (get-variable-entry symbol env t)))

(defmethod env:boundp
    (client
     (env virtual-run-time-environment)
     symbol)
  ;; SYMBOL-MACRO has a value in the variable cell, however it is not treated
  ;; as a bound variable (following behavior of other implementations). It is
  ;; not clearly defined what does bound mean in context of the symbol-macro,
  ;; but since it is expanded it is not a variable (so can't be bound).
  (alx:if-let ((entry (get-variable-entry symbol env)))
    (or (constant-variable entry)
        (special-variable entry))
    nil))

(defmethod env:constant-variable
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:if-let ((entry (get-variable-entry symbol env)))
    (values (constant-variable entry) (car (cell entry)))
    (values nil nil)))

(defmethod (setf env:constant-variable)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (let* ((entry (get-variable-entry symbol env t))
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
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:if-let ((entry (get-variable-entry symbol env)))
    (values (special-variable entry) (car (cell entry)))
    (values nil nil)))

(defmethod (setf env:special-variable)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol
     init-p)
  (let ((entry (get-variable-entry symbol env t)))
    (cond ((constant-variable entry)
           (error "~s is already defined as a constant." symbol))
          ((symbol-macro entry)
           (error "~s is already defined as a symbol macro." symbol))
          (t
           (setf (special-variable entry) t)
           (when init-p
             (setf (car (cell entry)) new-value))))))

(defmethod env:symbol-macro
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:if-let ((entry (get-variable-entry symbol env)))
    (if (symbol-macro entry)
        (let ((def (car (cell entry))))
          (values def (funcall def symbol env)))
        (values nil nil))
    (values nil nil)))

(defmethod (setf env:symbol-macro)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (let ((entry (get-variable-entry symbol env t)))
    (cond
      ((constant-variable entry)
       (error "~s is already defined as a constant." symbol))
      ((special-variable entry)
       (error "~s is already defined as a special variable." symbol))
      (t
       (setf (symbol-macro entry) t)
       (setf (car (cell entry)) (constantly new-value))))))

(defmethod env:variable-type
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:if-let ((entry (get-variable-entry symbol env)))
    (if (constant-variable entry)
        (type-of (car (cell entry)))
        (or (variable-type entry)
            t))
    t))

(defmethod (setf env:variable-type)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (let ((entry (get-variable-entry symbol env t)))
    (if (constant-variable entry)
        (error "Can't proclaim a type of a constant ~s." symbol)
        (setf (variable-type entry) new-value))))

(defmethod env:variable-unbound
    (client
     (env virtual-run-time-environment)
     symbol)
  +unbound+)

(defmethod env:variable-description
    (client
     (env virtual-run-time-environment)
     symbol)
  nil)

(defmethod env:type-expander
    (client
     (env virtual-run-time-environment)
     symbol)
  (alx:when-let ((entry (get-variable-entry symbol env)))
    (type-expander entry)))

(defmethod (setf env:type-expander)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (let ((entry (get-variable-entry symbol env t)))
    (setf (type-expander entry) new-value)))


;;; Other

(defmethod env:find-class
    (client
     (env virtual-run-time-environment)
     symbol)
  (values (gethash symbol (classes env))))

(defmethod (setf env:find-class)
    (new-value
     client
     (env virtual-run-time-environment)
     symbol)
  (if (null new-value)
      (remhash symbol (classes env))
      (setf (gethash symbol (classes env))
            new-value)))

(defmethod env:class-description
    (client
     (env virtual-run-time-environment)
     symbol)
  nil)

(defmethod env:find-package
    (client
     (env virtual-run-time-environment)
     name)
  (values (gethash name (packages env))))

(defmethod (setf env:find-package)
    (new-package
     client
     (env virtual-run-time-environment)
     name)
  (if (null new-package)
      (remhash name (packages env))
      (setf (gethash name (packages env)) new-package)))


;;; Declarations

(defmethod env:proclamation
    (client
     (env virtual-run-time-environment)
     name)
  (values (gethash name (declarations env))))

(defmethod (setf env:proclamation)
    (new-value
     client
     (env virtual-run-time-environment)
     name)
  (cond ((null new-value)
         (remhash name (declarations env)))
        (t
         (setf (gethash name (declarations env)) new-value))))

;;; Compilation environment

(defclass virtual-compilation-environment (env:compilation-environment)
  ((function-descriptions
    :initarg :function-descriptions
    :reader function-descriptions
    :initform (make-hash-table :test #'equal))
   (variable-descriptions
    :initarg :variable-descriptions
    :reader variable-descriptions
    :initform (make-hash-table :test #'eq))
   (class-descriptions
    :initarg :class-descriptions
    :reader class-descriptions
    :initform (make-hash-table :test #'eq))))

(defmethod env:function-description
    (client
     (env virtual-compilation-environment)
     function-name)
  (or (gethash function-name (function-descriptions env))
      (env:function-description client (env:parent env) function-name)))

(defmethod (setf function-description)
    (description
     client
     (env virtual-compilation-environment)
     function-name)
  (setf (gethash function-name (function-descriptions env))
        description))

(defmethod variable-description
    (client
     (env virtual-compilation-environment)
     symbol)
  (or (gethash symbol (variable-descriptions env))
      (env:variable-description client (env:parent env) symbol)))

(defmethod (setf variable-description)
    (description
     client
     (env virtual-compilation-environment)
     symbol)
  (setf (gethash symbol (variable-descriptions env))
        description))

(defmethod class-description
    (client
     (env virtual-compilation-environment)
     symbol)
  (or (gethash symbol (class-descriptions env))
      (env:class-description client (env:parent env) symbol)))

(defmethod (setf class-description)
    (description
     client
     (env virtual-compilation-environment)
     symbol)
  (setf (gethash symbol (class-descriptions env))
        description))
