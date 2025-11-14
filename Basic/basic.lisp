(cl:in-package #:clostrum-basic)

;;; Implementation of the Clostrum methods.

(defmethod sys:parent (client (env basic-environment))
  (declare (ignore client))
  (parent env))

(defmethod sys:operator-status (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (status entry))))
(defmethod (setf sys:operator-status)
    (new client (env basic-environment) name)
  (let ((entry (if (null new)
                   (operator-entry name env)
                   (ensure-operator-entry client name env))))
    (unless (null entry)
      (setf (status entry) new)))
  new)

(defmethod sys:operator-cell (client (environment basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name environment)))
    (if entry
        (cell entry)
        nil)))

(defmethod sys:ensure-operator-cell (client (environment basic-environment) name)
  (cell (ensure-operator-entry client name environment)))

(defmethod sys:compiler-macro-function (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (compiler-macro-function entry))))
(defmethod (setf sys:compiler-macro-function)
    (new-value client (environment basic-environment) name)
  (let ((entry (if (null new-value)
                   (operator-entry name environment)
                   (ensure-operator-entry client name environment))))
    (unless (null entry)
      (setf (compiler-macro-function entry) new-value)))
  new-value)

(defmethod sys:setf-expander (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (setf-expander entry))))
(defmethod (setf sys:setf-expander)
    (new-value client (environment basic-environment) name)
  (let ((entry (if (null new-value)
                   (operator-entry name environment)
                   (ensure-operator-entry client name environment))))
    (unless (null entry)
      (setf (setf-expander entry) new-value)))
  new-value)

(defmethod sys:operator-inline (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (inline entry))))
(defmethod (setf sys:operator-inline) (new client (env basic-environment) name)
  (let ((entry (ensure-operator-entry client name env)))
    (setf (inline-known-p entry) t (inline entry) new)))

(defmethod sys:operator-inline-known-p (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (inline-known-p entry))))

(defmethod sys:operator-inline-data (client (env basic-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name env)))
    (if (null entry)
        nil
        (inline-data entry))))
(defmethod (setf sys:operator-inline-data) (new client (env basic-environment) name)
  (setf (inline-data (ensure-operator-entry client name env)) new))

(defmethod sys:operator-ftype (client (env basic-environment) name)
  (let ((entry (operator-entry name env)))
    (if (null entry)
        (top-type client)
        (ftype entry))))
(defmethod (setf sys:operator-ftype)
    (new client (env basic-environment) name)
  (setf (ftype (ensure-operator-entry client name env)) new))


;;; Variables.

(defmethod sys:variable-status
    (client (environment basic-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (status entry))))
(defmethod (setf sys:variable-status)
    (new client (environment basic-environment) symbol)
  (let ((entry (if (null new)
                   (variable-entry symbol environment)
                   (ensure-variable-entry client symbol environment))))
    (unless (null entry)
      (setf (status entry) new))
    new))

(defmethod sys:variable-cell (client (environment basic-environment) name)
  (declare (ignore client))
  (let ((entry (variable-entry name environment)))
    (if entry
        (cell entry)
        nil)))

(defmethod sys:ensure-variable-cell
    (client (environment basic-environment) symbol)
  (cell (ensure-variable-entry client symbol environment)))

(defmethod sys:variable-macro-expander
    (client (environment basic-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (symbol-macro-expander entry))))
(defmethod (setf sys:variable-macro-expander)
    (new client (environment basic-environment) symbol)
  ;; NEW is always a function, as undefining a symbol macro is instead done by
  ;; changing its STATUS. So we don't need the (or ... (variable-entry ...))
  (setf (symbol-macro-expander (ensure-variable-entry client symbol environment)) new))

(defmethod sys:variable-type (client (environment basic-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if entry
        (vtype entry)
        (top-type client))))
(defmethod (setf sys:variable-type)
    (new client (environment basic-environment) symbol)
  (setf (vtype (ensure-variable-entry client symbol environment)) new))


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

(defmethod sys:type-cell (client (environment basic-environment) symbol)
  (declare (ignore client))
  (let ((entry (type-entry symbol environment)))
    (if entry
        (cell entry)
        nil)))

(defmethod sys:ensure-type-cell (client (environment basic-environment) symbol)
  (cell (ensure-type-entry client symbol environment)))

(defmethod sys:type-expander (client (environment basic-environment) symbol)
  (let ((entry (type-entry symbol environment)))
    (if (null entry)
        nil
        (type-expander entry))))
(defmethod (setf sys:type-expander)
    (new client (environment basic-environment) symbol)
  (let ((entry (if (null new)
                   (type-entry symbol environment)
                   (ensure-type-entry client symbol environment))))
    (unless (null entry)
      (setf (type-expander entry) new)))
  new)


;;; Declarations.

(defmethod sys:proclamation
    (client (environment basic-environment) name)
  (values (gethash name (declarations environment))))

(defmethod (setf sys:proclamation)
    (new-value client (environment basic-environment) name)
  (cond ((null new-value)
         (remhash name (declarations environment)))
        (t
         (setf (gethash name (declarations environment)) new-value))))


;;; Optimize.
(defmethod sys:optimize (client (environment basic-environment))
  (declare (ignore client))
  (optimize environment))
(defmethod (setf sys:optimize) (new client (environment basic-environment))
  (declare (ignore client))
  (setf (optimize environment) new))


;;; Methods for compilation environments.
(defmethod sys:symbol-plist-known-p
    (client (environment compilation-environment) symbol)
  (declare (ignore client symbol))
  nil)

(defmethod sys:find-package (client (environment compilation-environment) name)
  (declare (ignore client name))
  nil)

(defmethod sys:package-name
    (client (environment compilation-environment) package)
  (declare (ignore client package))
  nil)

(defmethod sys:package-names
    (client (environment compilation-environment) package)
  (declare (ignore client package))
  nil)

(defmethod sys:map-all-packages
    (client (environment compilation-environment) function)
  (declare (ignore client function)))
