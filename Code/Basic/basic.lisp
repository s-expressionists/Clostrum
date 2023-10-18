(cl:in-package #:clostrum-basic)

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
    :type (or function null)))
  (:default-initargs :name (error "The initarg :NAME is required.")))

;;; Make sure NAME names a function entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed
;;; either to MAKE-INSTANCE in order create a new entry if no entry
;;; exists, or will be passed to REINITIALIZE-INSTANCE to modify the
;;; existing entry if one does exist.  The existing entry or the entry
;;; being created is returned.
(defun ensure-operator-entry (name environment &rest keyword-arguments)
  (let ((entry (operator-entry name environment)))
    (if (null entry)
        (setf (operator-entry name environment)
              (apply #'make-instance 'operator-entry
                     :name name keyword-arguments))
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

(defclass variable-entry ()
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
   (plist
    :initform nil
    :accessor plist
    :type list))
  (:default-initargs :name (error "The initarg :NAME is required.")))

;;; Make sure NAME names a variable entry in ENVIRONMENT.
;;; KEYWORD-ARGUMENTS are keyword/value pairs that will be passed
;;; either to MAKE-INSTANCE in order create a new entry if no entry
;;; exists, or will be passed to REINITIALIZE-INSTANCE to modify the
;;; existing entry if one does exist.  The existing entry or the entry
;;; being created is returned.
(defun ensure-variable-entry (name environment &rest keyword-arguments)
  (let ((entry (variable-entry name environment)))
    (if (null entry)
        (setf (variable-entry name environment)
              (apply #'make-instance 'variable-entry
                     :name name keyword-arguments))
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

;;; Implementation of the Clostrum methods.

(declaim (inline cell-value (setf cell-value) cell-boundp cell-makunbound))
(defun cell-value (cell) (car cell))
(defun (setf cell-value) (new cell) (setf (car cell) new))
(defun cell-boundp (cell) (not (eq (car cell) (cdr cell))))
(defun cell-makunbound (cell) (setf (car cell) (cdr cell)) (values))

(defmethod sys:operator-cell-value (client cell)
  (declare (ignore client))
  (cell-value cell))
(defmethod (setf sys:operator-cell-value) (new client cell)
  (declare (ignore client))
  (setf (cell-value cell) new))
(defmethod sys:operator-cell-boundp (client cell)
  (declare (ignore client))
  (cell-boundp cell))
(defmethod sys:operator-cell-makunbound (client cell)
  (declare (ignore client))
  (cell-makunbound cell))

(defmethod sys:operator-status (client (env run-time-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (status entry))))
(defmethod (setf sys:operator-status)
    (new client (env run-time-environment) name)
  (declare (ignore client))
  (let ((entry (if (null new)
                   (operator-entry name env)
                   (ensure-operator-entry name env))))
    (unless (null entry)
      (setf (status entry) new)))
  new)

(defmethod sys:operator-cell (client (environment run-time-environment) name)
  (declare (ignore client))
  (cell (ensure-operator-entry name environment)))

(defmethod sys:compiler-macro-function (client (env run-time-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (compiler-macro-function entry))))
(defmethod (setf sys:compiler-macro-function)
    (new-value client (environment run-time-environment) name)
  (declare (ignore client))
  (let ((entry (if (null new-value)
                   (operator-entry name environment)
                   (ensure-operator-entry name environment))))
    (unless (null entry)
      (setf (compiler-macro-function entry) new-value)))
  new-value)

(defmethod sys:setf-expander (client (env run-time-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (setf-expander entry))))
(defmethod (setf sys:setf-expander)
    (new-value client (environment run-time-environment) name)
  (declare (ignore client))
  (let ((entry (if (null new-value)
                   (operator-entry name environment)
                   (ensure-operator-entry name environment))))
    (unless (null entry)
      (setf (setf-expander entry) new-value)))
  new-value)


;;; Variables.
(defmethod sys:variable-cell-value (client cell)
  (declare (ignore client))
  (cell-value cell))
(defmethod (setf sys:variable-cell-value) (new client cell)
  (declare (ignore client))
  (setf (cell-value cell) new))
(defmethod sys:variable-cell-boundp (client cell)
  (declare (ignore client))
  (cell-boundp cell))
(defmethod sys:variable-cell-makunbound (client cell)
  (declare (ignore client))
  (cell-makunbound cell))

(defmethod sys:variable-cell
    (client (environment run-time-environment) symbol)
  (cell (ensure-variable-entry symbol environment)))

(defmethod sys:variable-status
    (client (environment run-time-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (status entry))))
(defmethod (setf sys:variable-status)
    (new client (environment run-time-environment) symbol)
  (let ((entry (if (null new)
                   (variable-entry symbol environment)
                   (ensure-variable-entry symbol environment))))
    (unless (null entry)
      (setf (status entry) new))
    new))

(defmethod sys:variable-macro-expander
    (client (environment run-time-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (symbol-macro-expander entry))))
(defmethod (setf sys:variable-macro-expander)
    (new client (environment run-time-environment) symbol)
  ;; NEW is always a function, as undefining a symbol macro is instead done by
  ;; changing its STATUS. So we don't need the (or ... (variable-entry ...))
  (setf (symbol-macro-expander (ensure-variable-entry symbol environment)) new))

(defmethod sys:symbol-plist (client (environment run-time-environment) symbol)
  (declare (ignore client))
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (plist entry))))
(defmethod (setf sys:symbol-plist)
    (new client (environment run-time-environment) symbol)
  (declare (ignore client))
  (setf (plist (ensure-variable-entry symbol environment)) new))


;;; Types and classes.

(defmethod sys:type-cell-value (client cell)
  (declare (ignore client))
  (cell-value cell))
(defmethod (setf sys:type-cell-value) (new client cell)
  (declare (ignore client))
  (setf (cell-value cell) new))
(defmethod sys:type-cell-boundp (client cell)
  (declare (ignore client))
  (cell-boundp cell))
(defmethod sys:type-cell-makunbound (client cell)
  (declare (ignore client))
  (cell-makunbound cell))

(defmethod sys:type-cell (client (environment run-time-environment) symbol)
  (cell (ensure-type-entry symbol environment)))

(defmethod sys:type-expander (client (environment run-time-environment) symbol)
  (let ((entry (type-entry symbol environment)))
    (if (null entry)
        nil
        (type-expander entry))))
(defmethod (setf sys:type-expander)
    (new client (environment run-time-environment) symbol)
  (let ((entry (if (null new)
                   (type-entry symbol environment)
                   (ensure-type-entry symbol environment))))
    (unless (null entry)
      (setf (type-expander entry) new)))
  new)

;;; Packages.

(defmethod sys:find-package
    (client (environment run-time-environment) name)
  (values (gethash name (packages environment))))

(defmethod (setf sys:find-package)
    (new-package client (environment run-time-environment) name)
  (if (null new-package)
      (remhash name (packages environment))
      (setf (gethash name (packages environment)) new-package)))


;;; Declarations.

(defmethod sys:proclamation
    (client (environment run-time-environment) name)
  (values (gethash name (declarations environment))))

(defmethod (setf sys:proclamation)
    (new-value client (environment run-time-environment) name)
  (cond ((null new-value)
         (remhash name (declarations environment)))
        (t
         (setf (gethash name (declarations environment)) new-value))))
