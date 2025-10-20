(cl:in-package #:clostrum-basic)

(defgeneric top-type (client)
  (:method (client)
    (declare (ignore client))
    ;; Default: use type specifiers.
    t)
  (:documentation "Return the top type. Clients with custom type representations must specialize this method to return the appropriate representation.
The default method uses type specifiers as types, and thus returns T."))

;;; Function and variable entries.
(defclass operator-entry ()
  ((name
    :initarg :name
    :reader name)
   (status
    :initform nil
    :accessor status
    :type (member :function :macro :special-operator nil))
   ;; By default, cells are conses.
   ;; The CAR of the cell contains the function determined by the
   ;; entry.  The CDR of the cell contains a function that, when
   ;; called, signals an error.  When the function determined by the
   ;; entry is undefined, the CAR of the cell is the same as the CDR
   ;; of the cell.
   ;; The implementation of cells can be changed by clients by
   ;; specializing MAKE-OPERATOR-CELL as well as the CLOSTRUM-SYS
   ;; functions for manipulating cells.
   (cell
    :initarg :cell
    :reader cell)
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
                     :cell (make-operator-cell client name environment)
                     keyword-arguments))
        (apply #'reinitialize-instance entry keyword-arguments))))

(defgeneric make-operator-cell (client environment name)
  (:method (client env name)
    (declare (ignore client env))
    ;; We indicate that a function name is FUNBOUND by storing a
    ;; function in the CAR of the cell that, when called, signals an
    ;; UNDEFINED-FUNCTION error.  This way, there is no need for an
    ;; explicit test to verify that the name is FBOUND before calling
    ;; the function.  We store the same, as in EQ, function in the CDR
    ;; of the cell.  That way, testing whether the function is unbound
    ;; is an EQ comparison between the CAR and the CDR of the cell, and
    ;; FMAKUNBOUND is implemented by copying the CDR of the cell to the
    ;; CAR.
    (flet ((unbound-function (&rest args)
             (declare (ignore args))
             (error 'undefined-function :name name)))
      (cons #'unbound-function #'unbound-function)))
  (:documentation "Make a fresh operator cell. CLOSTRUM-SYS:OPERATOR-CELL-VALUE etc. should be specialized to operate correctly on the cell at least when given the same client.
Clients may specialize this method if they have their own implementations of cells. A default method is provided that uses a simple representation as a cons."))

(defconstant +unbound+ 'unbound)

(defclass compilation-variable-entry ()
  ((name
    :initarg :name
    :reader name)
   (status
    :initform nil
    :accessor status
    :type (member :constant :special :symbol-macro nil))
   ;; By default, cells are conses.
   ;; The CAR of the cell contains the value of the variable
   ;; determined by the entry.  The CDR of the cell contains a value
   ;; that indicates that the variable is unbound.  When the variable
   ;; is unbound, the CAR and the CDR contain the same value.
   ;; The nature of cells can be changed by clients by specializing
   ;; MAKE-VARIABLE-CELL as well as the CLOSTRUM-SYS cell functions.
   (cell
    :reader cell
    :initarg :cell
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
                        :cell (make-variable-cell client environment name)
                        keyword-arguments))
                (compilation-environment
                 (apply #'make-instance 'compilation-variable-entry
                        :name name :vtype (top-type client)
                        :cell (make-variable-cell client environment name)
                        keyword-arguments))))
        (apply #'reinitialize-instance entry keyword-arguments))))

(defgeneric make-variable-cell (client environment name)
  (:method (client environment name)
    (declare (ignore client environment name))
    (cons +unbound+ +unbound+))
  (:documentation "Make a fresh variable cell. CLOSTRUM-SYS:VARIABLE-CELL-VALUE etc. should be specialized to operate correctly on the cell at least when given the same client.
Clients may specialize this method if they have their own implementations of cells. A default method is provided that uses a simple representation as a cons."))

(defclass type-entry ()
  ((%name :initarg :name :reader name)
   (%cell :initarg :cell :reader cell :type cons)
   (%type-expander :initform nil :accessor type-expander
                   :type (or function null))))

;;; Make sure NAME names a type entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed either
;;; to MAKE-INSTANCE in order to create a new entry if none exits,
;;; or to REINITIALIZE-INSTANCE to modify an existing entry.
;;; The new or exiting entry is returned.
(defun ensure-type-entry (client name environment &rest keyword-arguments)
  (let ((entry (type-entry name environment)))
    (if (null entry)
        (setf entry (apply #'make-instance 'type-entry :name name
                           :cell (make-type-cell client environment name)
                           keyword-arguments)
              (type-entry name environment) entry)
        (apply #'reinitialize-instance entry keyword-arguments))
    entry))

(defgeneric make-type-cell (client environment name)
  (:method (client environment name)
    (declare (ignore client environment name))
    (cons nil nil))
  (:documentation "Make a fresh type cell. CLOSTRUM-SYS:TYPE-CELL-VALUE etc. should be specialized to operate correctly on the cell at least when given the same client.
Clients may specialize this method if they have their own implementations of cells. A default method is provided that uses a simple representation as a cons."))

(declaim (cl:inline cell-value (setf cell-value) cell-boundp cell-makunbound))
(defun cell-value (cell) (car cell))
(defun (setf cell-value) (new cell) (setf (car cell) new))
(defun cell-boundp (cell) (not (eq (car cell) (cdr cell))))
(defun cell-makunbound (cell) (setf (car cell) (cdr cell)) (values))
