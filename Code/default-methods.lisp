(cl:in-package #:clostrum-implementation)

;;; Operators

(defmethod env:fdefinition (client environment operator-name)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (if (sys:operator-cell-boundp client cell)
        (sys:operator-cell-value client cell)
        (error 'undefined-function :name operator-name))))

(defmethod (setf env:fdefinition) (new client environment operator-name)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (setf (sys:operator-status client environment operator-name) :function
          (sys:operator-cell-value client cell) new)))

(defmethod env:fboundp (client environment operator-name)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (sys:operator-cell-boundp client cell)))

(defmethod env:fmakunbound (client environment operator-name)
  ;; NOTE: We do not forbid fmakunbound of special operators. If the client
  ;; wants to forbid that, it is assumed they will roll that into a general
  ;; package lock mechanism that forbids undefining standard functions and
  ;; macros as well, and that's out of scope for Clostrum.
  ;; Similar considerations apply to (setf fdefinition), etc.
  (setf (sys:operator-status client environment operator-name) nil)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (sys:operator-cell-makunbound client cell))
  operator-name)

(defmethod env:macro-function (client environment operator-name)
  (if (eq (sys:operator-status client environment operator-name) :macro)
      (let ((cell (sys:operator-cell client environment operator-name)))
        (if (sys:operator-cell-boundp client cell)
            (sys:operator-cell-value client cell)
            nil))
      nil))

(defmethod (setf env:macro-function) (new client environment operator-name)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (setf (sys:operator-status client environment operator-name) :macro
          (sys:operator-cell-value client cell) new)))

(defmethod env:special-operator-p (client environment operator-name)
  (eq (sys:operator-status client environment operator-name) :special-operator))

(defmethod env:make-special-operator (client environment operator-name new)
  (let ((cell (sys:operator-cell client environment operator-name)))
    (setf (sys:operator-cell-value client cell) new
          (sys:operator-status client environment operator-name) :special-operator)
    operator-name))

(defmethod env:setf-expander (client environment operator-name)
  (sys:setf-expander client environment operator-name))

(defmethod (setf env:setf-expander) (new client environment operator-name)
  (case (sys:operator-status client environment operator-name)
    ((:function :macro)
     (setf (sys:setf-expander client environment operator-name) new))
    (otherwise
     (error 'env:attempt-to-define-a-setf-expander-of-non-existing-function-or-macro
            :name operator-name))))

;;; Variables

(defmethod env:symbol-value (client environment variable-name)
  (let ((cell (sys:variable-cell client environment variable-name)))
    (if (sys:variable-cell-boundp client cell)
        (sys:variable-cell-value client cell)
        (error 'unbound-variable :name variable-name))))

(defmethod (setf env:symbol-value) (new client environment variable-name)
  (if (eq (sys:variable-status client environment variable-name) :constant)
      (error 'env:attempt-to-set-constant-value :name variable-name)
      (let ((cell (sys:variable-cell client environment variable-name)))
        (setf (sys:variable-cell-value client cell) new))))

(defmethod env:boundp (client environment variable-name)
  (sys:variable-cell-boundp
   client (sys:variable-cell client environment variable-name)))

(defmethod env:makunbound (client environment variable-name)
  (sys:variable-cell-makunbound
   client (sys:variable-cell client environment variable-name)))

(defmethod env:make-variable (client environment variable-name
                              &optional (value nil valuep))
  (ecase (sys:variable-status client environment variable-name)
    ((nil)
     (when valuep
       (setf (sys:variable-cell-value
              client (sys:variable-cell client environment variable-name))
             value))
     (setf (sys:variable-status client environment variable-name) :special))
    ((:special)
     (when valuep
       (let ((cell (sys:variable-cell client environment variable-name)))
         (unless (sys:variable-cell-boundp client cell)
           (setf (sys:variable-cell-value client cell) value)))))
    ((:constant)
     (error 'env:attempt-to-define-special-variable-for-existing-constant
            :name variable-name))
    ((:symbol-macro)
     (error 'env:attempt-to-define-special-variable-for-existing-symbol-macro
            :name variable-name))))

(defmethod env:make-parameter (client environment variable-name new)
  (ecase (sys:variable-status client environment variable-name)
    ((nil)
     (setf (sys:variable-cell-value
            client (sys:variable-cell client environment variable-name))
           new
           (sys:variable-status client environment variable-name)
           :special))
    ((:special)
     (let ((cell (sys:variable-cell client environment variable-name)))
       (setf (sys:variable-cell-value client cell) new)))
    ((:constant)
     (error 'env:attempt-to-define-special-variable-for-existing-constant
            :name variable-name))
    ((:symbol-macro)
     (error 'env:attempt-to-define-special-variable-for-existing-symbol-macro
            :name variable-name))))

(defmethod env:make-constant (client environment variable-name new)
  (ecase (sys:variable-status client environment variable-name)
    ((nil) ; undefined: this is a new constant
     (setf (sys:variable-cell-value
            client (sys:variable-cell client environment variable-name))
           new
           (sys:variable-status client environment variable-name)
           :constant))
    ((:constant)
     (let ((old
             ;; The cell must be bound, as constants always are.
             (sys:variable-cell-value
              client (sys:variable-cell client environment variable-name))))
       (unless (eql old new)
         ;; TODO: Restarts?
         (error 'env:attempt-to-redefine-constant-incompatibly
                :name variable-name :old old :new new))
       new))
    ((:special)
     (error 'env:attempt-to-define-constant-for-existing-special-variable
            :name variable-name))
    ((:symbol-macro)
     (error 'env:attempt-to-define-constant-for-existing-symbol-macro
            :name variable-name))))

(defun %make-symbol-macro-expander (expansion)
  (lambda (form env)
    (declare (ignore form env))
    expansion))

(defmethod env:make-symbol-macro (client environment variable-name expansion)
  (ecase (sys:variable-status client environment variable-name)
    ((nil :symbol-macro)
     (setf (sys:variable-macro-expander client environment variable-name)
           (%make-symbol-macro-expander expansion)
           (sys:variable-status client environment variable-name)
           :symbol-macro))
    ((:special)
     (error 'env:attempt-to-define-symbol-macro-for-existing-special-variable
            :name variable-name))
    ((:constant)
     (error 'env:attempt-to-define-symbol-macro-for-existing-constant
            :name variable-name))))

;;; Types and classes

(defmethod env:find-class (client environment class-name &optional (errorp t))
  (let ((cell (sys:type-cell client environment class-name)))
    (cond ((sys:type-cell-boundp client cell)
           (sys:type-cell-value client cell))
          (errorp (error 'env:undefined-class :name class-name))
          (t nil))))

(defmethod (setf env:find-class)
    (new client environment class-name &optional errorp)
  (declare (ignore errorp))
  (setf (sys:type-expander client environment class-name) nil)
  (cond ((null new)
         (sys:type-cell-makunbound
          client (sys:type-cell client environment class-name)))
        (t
         (setf (sys:type-cell-value
                client (sys:type-cell client environment class-name))
               new)))
  new)

(defmethod env:make-type (client environment type-name expander)
  (setf (sys:type-expander client environment type-name) expander)
  (sys:type-cell-makunbound client
                            (sys:type-cell client environment type-name))
  type-name)

(defmethod env:type-expand-1 (client environment (type-specifier symbol))
  (let ((expander (env:type-expander client environment type-specifier)))
    (if (null expander)
        (values type-specifier nil)
        (values (funcall expander type-specifier environment) t))))

(defmethod env:type-expand-1 (client environment (type-specifier cons))
  (let ((operator (car type-specifier)))
    (if (symbolp operator)
        (let ((expander (env:type-expander client environment operator)))
          (if (null expander)
              (values type-specifier nil)
              (values (funcall expander type-specifier environment) t)))
        (values type-specifier nil))))

(defmethod env:type-expand-1 (client environment (type-specifier class))
  (declare (ignore client environment))
  type-specifier)

(defmethod env:type-expand (client environment type-specifier)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp)
               (env:type-expand-1 client environment type-specifier)
             (if expandedp
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded))))))

;;; Combination

(defmethod env:macroexpand-1 (client environment (form symbol))
  (if (eq (sys:variable-status client environment form) :symbol-macro)
      (let ((expander (sys:variable-macro-expander client environment form)))
        (values (funcall *macroexpand-hook* expander form environment) t))
      (values form nil)))

(defmethod env:macroexpand-1 (client environment (form cons))
  (let ((operator (car form)))
    (if (symbolp operator)
        (let ((expander (env:macro-function client environment operator)))
          (if expander
              (values (funcall *macroexpand-hook* expander form environment) t)
              (values form nil)))
        (values form nil))))

(defmethod env:macroexpand-1 (client environment (form t))
  (declare (ignore client environment))
  (values form nil))

(defmethod env:macroexpand (client environment form)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp)
               (env:macroexpand-1 client environment form)
             (if expandedp
                 (setf ever-expanded t form expansion)
                 (return (values form ever-expanded))))))

(defmethod env:constantp (client environment (form symbol))
  (case (sys:variable-status client environment form)
    ((:constant) t)
    ((:symbol-macro)
     (env:constantp
      client environment (env:macroexpand-1 client environment form)))
    (otherwise nil)))

(defmethod env:constantp (client environment (form cons))
  ;; This method is kind of awkward: A client must take care of QUOTE itself
  ;; to use this function for cl:constantp. Clostrum does not assume that QUOTE
  ;; always has its standard meaning, and because it imposes no meaning on
  ;; special operator values (returned by fdefinition), it has no way to check
  ;; that QUOTE has its standard meaning.
  nil)

(defmethod env:constantp (client environment (form t))
  ;; not a symbol or cons. therefore the form is self-evaluating.
  t)
