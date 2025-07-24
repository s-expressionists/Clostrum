(cl:in-package #:clostrum-basic)

(defgeneric top-type (client)
  (:method (client)
    (declare (ignore client))
    ;; Default: use type specifiers.
    t))

;;; Function and variable entries.
(defclass operator-entry ()
  ((name
    :initarg :name
    :reader name)
   (status
    :initform nil
    :accessor status
    :type (member :function :macro :special-operator nil))
   ;; The CAR of the cell contains the function determined by the
   ;; entry.  The CDR of the cell contains a function that, when
   ;; called, signals an error.  When the function determined by the
   ;; entry is undefined, the CAR of the cell is the same as the CDR
   ;; of the cell.
   (cell
    :reader cell
    :type cons)
   (compiler-macro-function
    :initform nil
    :accessor compiler-macro-function
    :type (or function null))
   (setf-expander
    :initform nil
    :accessor setf-expander
    :type (or function null))
   (ftype
    :initarg :ftype
    :accessor ftype)
   (inline
    :initform nil
    :accessor inline)
   (inline-known-p
    :initform nil
    :accessor inline-known-p)
   (inline-data
    :initform nil
    :accessor inline-data))
  (:default-initargs :name (error "The initarg :NAME is required.")
                     :ftype (error "The initarg :FTYPE is required.")))

;;; Make sure NAME names a function entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed
;;; either to MAKE-INSTANCE in order create a new entry if no entry
;;; exists, or will be passed to REINITIALIZE-INSTANCE to modify the
;;; existing entry if one does exist.  The existing entry or the entry
;;; being created is returned.
(defun ensure-operator-entry (client name environment &rest keyword-arguments)
  (let ((entry (operator-entry name environment)))
    (if (null entry)
        (setf (operator-entry name environment)
              (apply #'make-instance 'operator-entry
                     :name name :ftype (top-type client)
                     keyword-arguments))
        (apply #'reinitialize-instance entry keyword-arguments))))

(defmethod initialize-instance :after ((instance operator-entry) &key name)
  ;; We indicate that a function name is FUNBOUND by storing a
  ;; function in the CAR of the cell that, when called, signals an
  ;; UNDEFINED-FUNCTION error.  This way, there is no need for an
  ;; explicit test to verify that the name is FBOUND before calling
  ;; the function.  We store the same, as in EQ, function in the CDR
  ;; of the cell.  That way, testing whether the function is unbound
  ;; is an EQ comparison between the CAR and the CDR of the cell, and
  ;; FMAKUNBOUND is implemented by copying the CDR of the cell to the
  ;; CAR.
  (let ((unbound-function
          (lambda (&rest args)
            (declare (ignore args))
            (error 'undefined-function :name name))))
    (setf (slot-value instance 'cell)
          (cons unbound-function unbound-function))))

(defun function-bound-p (operator-entry)
  (let ((cell (cell operator-entry)))
    (not (eq (car cell) (cdr cell)))))

(defconstant +unbound+ 'unbound)

(defclass compilation-variable-entry ()
  ((name
    :initarg :name
    :reader name)
   (status
    :initform nil
    :accessor status
    :type (member :constant :special :symbol-macro nil))
   ;; The CAR of the cell contains the value of the variable
   ;; determined by the entry.  The CDR of the cell contains a value
   ;; that indicates that the variable is unbound.  When the variable
   ;; is unbound, the CAR and the CDR contain the same value.  Since
   ;; CL:MAKUNBOUND (which should really be called something else like
   ;; MAKE-TO-HAVE-NO-VALUE) must take into account dynamic bindings
   ;; of the variable, we do not supply code for MAKUNBOUND here.  It
   ;; must be implemented by the client.
   (cell
    :reader cell
    :initform (cons +unbound+ +unbound+)
    :type cons)
   (symbol-macro-expander
    :accessor symbol-macro-expander
    :type (or function null))
   (vtype
    :initarg :vtype
    :accessor vtype))
  (:default-initargs :name (error "The initarg :NAME is required.")
                     :vtype (error "The initarg :VTYPE is required.")))

(defclass variable-entry (compilation-variable-entry)
  ((plist
    :initform nil
    :accessor plist
    :type list)
   ;; This is necessary to ensure that inheritance of symbol plists
   ;; works properly. If known-p is true, this environment has a plist,
   ;; so parents shouldn't be consulted.
   ;; The separate variable is necessary because it is possible for an
   ;; environment to have a plist set to NIL while parent plists exist
   ;; and are non-null.
   (plist-known-p
    :initform nil
    :accessor plist-known-p)))

;;; Make sure NAME names a variable entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed
;;; either to MAKE-INSTANCE in order create a new entry if no entry
;;; exists, or will be passed to REINITIALIZE-INSTANCE to modify the
;;; existing entry if one does exist.  The existing entry or the entry
;;; being created is returned.
(defun ensure-variable-entry (client name environment &rest keyword-arguments)
  (let ((entry (variable-entry name environment)))
    (if (null entry)
        (setf (variable-entry name environment)
              (etypecase environment
                (run-time-environment
                 (apply #'make-instance 'variable-entry
                        :name name :vtype (top-type client)
                        keyword-arguments))
                (compilation-environment
                 (apply #'make-instance 'compilation-variable-entry
                        :name name :vtype (top-type client)
                        keyword-arguments))))
        (apply #'reinitialize-instance entry keyword-arguments))))

(defun variable-bound-p (variable-entry)
  (let ((cell (cell variable-entry)))
    (not (eq (car cell) +unbound+))))

(defclass type-entry ()
  ((%name :initarg :name :reader name)
   (%cell :initform (cons nil nil) :reader cell :type cons)
   (%type-expander :initform nil :accessor type-expander
                   :type (or function null))))

;;; Make sure NAME names a type entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed either
;;; to MAKE-INSTANCE in order to create a new entry if none exits,
;;; or to REINITIALIZE-INSTANCE to modify an existing entry.
;;; The new or exiting entry is returned.
(defun ensure-type-entry (name environment &rest keyword-arguments)
  (let ((entry (type-entry name environment)))
    (if (null entry)
        (setf entry (apply #'make-instance 'type-entry :name name
                           keyword-arguments)
              (type-entry name environment) entry)
        (apply #'reinitialize-instance entry keyword-arguments))
    entry))

(declaim (cl:inline cell-value (setf cell-value) cell-boundp cell-makunbound))
(defun cell-value (cell) (car cell))
(defun (setf cell-value) (new cell) (setf (car cell) new))
(defun cell-boundp (cell) (not (eq (car cell) (cdr cell))))
(defun cell-makunbound (cell) (setf (car cell) (cdr cell)) (values))
