(cl:in-package #:clostrum-basic)

(defclass run-time-environment (env:run-time-environment)
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

(defun function-entry (name env)
  (gethash name (functions env) nil))

(defun (setf function-entry) (new-entry name environment)
  (setf (gethash name (functions environment)) new-entry))

(defun variable-entry (name env)
  (gethash name (variables env) nil))

(defun (setf variable-entry) (new-entry name environment)
  (setf (gethash name (variables environment)) new-entry))

(defun class-entry (name environment)
  (gethash name (classes environment)))

(defun (setf class-entry) (new-entry name environment)
  (setf (gethash name (classes environment)) new-entry))
