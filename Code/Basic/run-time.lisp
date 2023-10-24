(cl:in-package #:clostrum-basic)

;;; Implementation of the run-time-specific Clostrum methods.

;;; Operators.

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

(defmethod sys:operator-cell (client (environment run-time-environment) name)
  (declare (ignore client))
  (let ((entry (operator-entry name environment)))
    (if entry
        (cell entry)
        nil)))

(defmethod sys:ensure-operator-cell (client (environment run-time-environment) name)
  (cell (ensure-operator-entry client name environment)))


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

(defmethod sys:variable-cell (client (environment run-time-environment) name)
  (declare (ignore client))
  (let ((entry (variable-entry name environment)))
    (if entry
        (cell entry)
        nil)))

(defmethod sys:ensure-variable-cell
    (client (environment run-time-environment) symbol)
  (cell (ensure-variable-entry client symbol environment)))

(defmethod sys:symbol-plist (client (environment run-time-environment) symbol)
  (declare (ignore client))
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (plist entry))))
(defmethod (setf sys:symbol-plist)
    (new client (environment run-time-environment) symbol)
  (let ((entry (ensure-variable-entry client symbol environment)))
    (setf (plist-known-p entry) t (plist entry) new)))

(defmethod sys:symbol-plist-known-p
    (client (environment run-time-environment) symbol)
  (let ((entry (variable-entry symbol environment)))
    (if (null entry)
        nil
        (plist-known-p entry))))


;;; Packages.

(defmethod sys:find-package
    (client (environment run-time-environment) name)
  (values (gethash name (packages environment))))

(defmethod (setf sys:find-package)
    (new-package client (environment run-time-environment) name)
  (if (null new-package)
      (remhash name (packages environment))
      (setf (gethash name (packages environment)) new-package)))

(defmethod sys:map-all-packages
    (client (environment run-time-environment) function)
  (maphash (lambda (name package)
             (declare (ignore name))
             (funcall function package))
           (packages environment)))
