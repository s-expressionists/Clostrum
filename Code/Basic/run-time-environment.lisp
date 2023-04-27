(cl:in-package #:clostrum-basic)

(defclass run-time-environment ()
  ((functions
    :reader functions
    :initform (make-hash-table :test #'equal))
   (variables
    :reader variables
    :initform (make-hash-table :test #'eq))
   (types
    :reader types
    :initform (make-hash-table :test #'eq))
   (packages
    :reader packages
    :initform (make-hash-table :test #'equal))
   (declarations
    :reader declarations
    :initform (make-hash-table :test #'eq))))

(defun operator-entry (name env)
  (gethash name (functions env) nil))

(defun (setf operator-entry) (new-entry name environment)
  (setf (gethash name (functions environment)) new-entry))

(defun variable-entry (name env)
  (gethash name (variables env) nil))

(defun (setf variable-entry) (new-entry name environment)
  (setf (gethash name (variables environment)) new-entry))

(defun type-entry (name environment)
  (gethash name (types environment)))

(defun (setf type-entry) (new-entry name environment)
  (setf (gethash name (types environment)) new-entry))
