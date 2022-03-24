(cl:in-package #:clostrum-basic)

(defclass compilation-environment (env:compilation-environment)
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
     (env compilation-environment)
     function-name)
  (or (gethash function-name (function-descriptions env))
      (env:function-description client (env:parent env) function-name)))

(defmethod (setf env:function-description)
    (description
     client
     (env compilation-environment)
     function-name)
  (setf (gethash function-name (function-descriptions env))
        description))

(defmethod env:variable-description
    (client
     (env compilation-environment)
     symbol)
  (or (gethash symbol (variable-descriptions env))
      (env:variable-description client (env:parent env) symbol)))

(defmethod (setf env:variable-description)
    (description
     client
     (env compilation-environment)
     symbol)
  (setf (gethash symbol (variable-descriptions env))
        description))

(defmethod env:class-description
    (client
     (env compilation-environment)
     symbol)
  (or (gethash symbol (class-descriptions env))
      (env:class-description client (env:parent env) symbol)))

(defmethod (setf env:class-description)
    (description
     client
     (env compilation-environment)
     symbol)
  (setf (gethash symbol (class-descriptions env))
        description))
