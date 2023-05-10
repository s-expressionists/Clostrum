(cl:in-package #:clostrum-basic)

(defclass compilation-environment ()
  ((%parent
    :initarg :parent
    :reader sys:evaluation-environment)
   (function-descriptions
    :initarg :function-descriptions
    :reader function-descriptions
    :initform (make-hash-table :test #'equal))
   (variable-descriptions
    :initarg :variable-descriptions
    :reader variable-descriptions
    :initform (make-hash-table :test #'eq))
   (type-descriptions
    :initarg :type-descriptions
    :reader type-descriptions
    :initform (make-hash-table :test #'eq))))

(defmethod sys:function-description
    (client
     (env compilation-environment)
     function-name)
  (or (gethash function-name (function-descriptions env))
      (sys:function-description client (sys:evaluation-environment env) function-name)))

(defmethod (setf sys:function-description)
    (description
     client
     (env compilation-environment)
     function-name)
  (setf (gethash function-name (function-descriptions env))
        description))

(defmethod sys:variable-description
    (client
     (env compilation-environment)
     symbol)
  (or (gethash symbol (variable-descriptions env))
      (sys:variable-description client (sys:evaluation-environment env) symbol)))

(defmethod (setf sys:variable-description)
    (description
     client
     (env compilation-environment)
     symbol)
  (setf (gethash symbol (variable-descriptions env))
        description))

(defmethod sys:type-description
    (client
     (env compilation-environment)
     symbol)
  (or (gethash symbol (type-descriptions env))
      (sys:type-description client (sys:evaluation-environment env) symbol)))

(defmethod (setf sys:type-description)
    (description
     client
     (env compilation-environment)
     symbol)
  (setf (gethash symbol (type-descriptions env))
        description))
