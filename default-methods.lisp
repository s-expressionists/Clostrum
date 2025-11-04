(cl:in-package #:clostrum-implementation)

(defmethod env:merge-types (client type1 type2)
  (declare (ignore client))
  ;; by default, assume that they are just type expanders.
  (cond ((eql type1 't) type2)
        ((eql type2 't) type1)
        (t `(and ,type1 ,type2))))

(defmethod env:merge-optimize (client new-optimize old-optimize)
  (declare (ignore client))
  ;; by default, assume only standard qualities, and that the optimizes are raw.
  (loop for quality in '(speed space safety debug compilation-speed)
        if (member quality new-optimize)
          collect `(,quality ,3)
        else if (assoc quality new-optimize)
               collect it
        else if (member quality old-optimize)
               collect `(,quality ,3)
        else if (assoc quality old-optimize)
               collect it))

;;; Operators

(defun find-operator-cell (client environment operator-name)
  "Find an operator cell if it exists, or return NIL. Internal."
  (or (sys:operator-cell client environment operator-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (find-operator-cell client parent operator-name)
          nil))))

(defmethod env:operator-status (client environment operator-name)
  (or (sys:operator-status client environment operator-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (env:operator-status client parent operator-name)
          nil))))

(defmethod env:ensure-operator-cell (client environment operator-name)
  (or (sys:operator-cell client environment operator-name)
    (let* ((parent (env:parent client environment))
           (parent-cell
             (if parent
                 (find-operator-cell client parent operator-name)
                 nil))
           (new-cell
             (sys:ensure-operator-cell client environment operator-name)))
      (when parent-cell
        ;; Copy in the old value.
        (when (sys:operator-cell-boundp client parent-cell)
          (setf (sys:operator-cell-value client new-cell)
                (sys:operator-cell-value client parent-cell))))
      new-cell)))

(defmethod env:fdefinition (client environment operator-name)
  (let ((cell (find-operator-cell client environment operator-name)))
    (if (and cell (sys:operator-cell-boundp client cell))
        (sys:operator-cell-value client cell)
        (error 'undefined-function :name operator-name))))

(defmethod (setf env:fdefinition)
    (new client (environment env:run-time-environment) operator-name)
  (let ((cell (env:ensure-operator-cell client environment operator-name)))
    (setf (sys:operator-status client environment operator-name) :function
          (sys:operator-cell-value client cell) new)))

(defmethod env:fboundp (client environment operator-name)
  (let ((cell (find-operator-cell client environment operator-name)))
    (and cell (sys:operator-cell-boundp client cell))))

(defmethod env:fmakunbound
    (client (environment env:run-time-environment) operator-name)
  ;; NOTE: We do not forbid fmakunbound of special operators. If the client
  ;; wants to forbid that, it is assumed they will roll that into a general
  ;; package lock mechanism that forbids undefining standard functions and
  ;; macros as well, and that's out of scope for Clostrum.
  ;; Similar considerations apply to (setf fdefinition), etc.
  (setf (sys:operator-status client environment operator-name) nil)
  ;; We do ENSURE-OPERATOR-CELL so that fmakunbound cannot affect
  ;; any ancestral environments.
  (let ((cell (env:ensure-operator-cell client environment operator-name)))
    (sys:operator-cell-makunbound client cell))
  operator-name)

(defmethod env:macro-function (client environment operator-name)
  (if (eq (env:operator-status client environment operator-name) :macro)
      (let ((cell (find-operator-cell client environment operator-name)))
        (if (and cell (sys:operator-cell-boundp client cell))
            (sys:operator-cell-value client cell)
            nil))
      nil))
(defmethod (setf env:macro-function) (new client environment operator-name)
  (let ((cell (env:ensure-operator-cell client environment operator-name)))
    (setf (sys:operator-status client environment operator-name) :macro
          (sys:operator-cell-value client cell) new)))

(defmethod env:compiler-macro-function (client environment operator-name)
  (or (sys:compiler-macro-function client environment operator-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (env:compiler-macro-function client parent operator-name)
          nil))))
(defmethod (setf env:compiler-macro-function) (new client environment operator-name)
  (setf (sys:compiler-macro-function client environment operator-name) new))

(defmethod env:special-operator-p (client environment operator-name)
  (eq (env:operator-status client environment operator-name) :special-operator))

(defmethod env:make-special-operator (client environment operator-name new)
  (let ((cell (env:ensure-operator-cell client environment operator-name)))
    (setf (sys:operator-cell-value client cell) new
          (sys:operator-status client environment operator-name) :special-operator)
    operator-name))

(defmethod env:note-function (client environment operator-name)
  (let ((status (sys:operator-status client environment operator-name)))
    (ecase status
      ((nil)
       (setf (sys:operator-status client environment operator-name) :function))
      ((:function))
      ((:macro :special-operator)
       (error 'env:attempt-to-note-operator-as-function
              :name operator-name :status status)))))

(defmethod env:operator-ftype (client environment operator-name)
  (let ((parent (env:parent client environment))
        (ftype (sys:operator-ftype client environment operator-name)))
    (if parent
        (env:merge-types client ftype (env:operator-ftype client parent operator-name))
        ftype)))
(defmethod (setf env:operator-ftype) (new client environment operator-name)
  (let ((status (env:operator-status client environment operator-name)))
    (if (eq :function status)
        (setf (sys:operator-ftype client environment operator-name) new)
        (error 'env:attempt-to-set-ftype-of-non-function
               :name operator-name :status status))))

(defmethod env:operator-inline (client environment operator-name)
  (if (sys:operator-inline-known-p client environment operator-name)
      (sys:operator-inline client environment operator-name)
      (let ((parent (env:parent client environment)))
        (if parent
            (env:operator-inline client parent operator-name)
            nil))))
(defmethod (setf env:operator-inline) (new client environment operator-name)
  (setf (sys:operator-inline client environment operator-name) new))

(defmethod env:operator-inline-data (client environment operator-name)
  (or (sys:operator-inline-data client environment operator-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (env:operator-inline-data client parent operator-name)
          nil))))
(defmethod (setf env:operator-inline-data) (new client environment operator-name)
  (setf (sys:operator-inline-data client environment operator-name) new))

(defmethod env:setf-expander (client (environment env:compilation-environment) operator-name)
  (let ((parent (env:parent client environment)))
    (if parent
        (env:setf-expander client parent operator-name)
        nil)))
(defmethod env:setf-expander (client (environment env:run-time-environment) operator-name)
  (or (sys:setf-expander client environment operator-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (env:setf-expander client parent operator-name)
          nil))))

(defmethod (setf env:setf-expander) (new client environment operator-name)
  (case (env:operator-status client environment operator-name)
    ;; CLHS says DEFINE-SETF-EXPANDER only works on function and macro names, but
    ;; we loosen that restriction a bit so that a setf expander can be defined for
    ;; THE. If an environment wants to restrict DEFINE-SETF-EXPANDER it can do so
    ;; just as well in the definition of DEFINE-SETF-EXPANDER, not the primitive
    ;; accessor here.
    ((:function :macro :special-operator)
     (setf (sys:setf-expander client environment operator-name) new))
    (otherwise
     (error 'env:attempt-to-define-a-setf-expander-of-non-existing-function-or-macro
            :name operator-name))))

;;; Variables

(defun find-variable-cell (client environment variable-name)
  "Find an variable cell if it exists, or return NIL. Internal."
  (or (sys:variable-cell client environment variable-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (find-variable-cell client parent variable-name)
          nil))))

(defmethod env:variable-status (client environment variable-name)
  (or (sys:variable-status client environment variable-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (env:variable-status client parent variable-name)
          nil))))

(defmethod env:ensure-variable-cell (client environment variable-name)
  (or (sys:variable-cell client environment variable-name)
    (let* ((parent (env:parent client environment))
           (parent-cell
             (if parent
                 (find-variable-cell client parent variable-name)
                 nil))
           (new-cell
             (sys:ensure-variable-cell client environment variable-name)))
      (when parent-cell
        ;; Copy in the old value.
        (when (sys:variable-cell-boundp client parent-cell)
          (setf (sys:variable-cell-value client new-cell)
                (sys:variable-cell-value client parent-cell))))
      new-cell)))

(defmethod env:symbol-value (client environment variable-name)
  (let ((cell (find-variable-cell client environment variable-name)))
    (if (and cell (sys:variable-cell-boundp client cell))
        (sys:variable-cell-value client cell)
        (error 'unbound-variable :name variable-name))))

(defmethod (setf env:symbol-value)
    (new client (environment env:run-time-environment) variable-name)
  (if (eq (env:variable-status client environment variable-name) :constant)
      (error 'env:attempt-to-set-constant-value :name variable-name)
      (let ((cell (env:ensure-variable-cell client environment variable-name)))
        (setf (sys:variable-cell-value client cell) new))))

(defmethod env:boundp (client environment variable-name)
  (let ((cell (sys:variable-cell client environment variable-name)))
    (and cell (sys:variable-cell-boundp client cell))))

(defmethod env:makunbound (client (environment env:run-time-environment) variable-name)
  (sys:variable-cell-makunbound
   client (env:ensure-variable-cell client environment variable-name)))

(defmethod env:variable-type (client environment variable-name)
  (let ((parent (env:parent client environment))
        (type (sys:variable-type client environment variable-name)))
    (if parent
        (env:merge-types client type (env:variable-type client parent variable-name))
        type)))
(defmethod (setf env:variable-type) (new client environment variable-name)
  (setf (sys:variable-type client environment variable-name) new))

(defmethod env:make-variable (client environment variable-name &optional value)
  (declare (ignore value))
  (ecase (env:variable-status client environment variable-name)
    ((nil)
     (setf (sys:variable-status client environment variable-name) :special))
    ((:special))
    ((:constant)
     (error 'env:attempt-to-define-special-variable-for-existing-constant
            :name variable-name))
    ((:symbol-macro)
     (error 'env:attempt-to-define-special-variable-for-existing-symbol-macro
            :name variable-name))))

(defmethod env:make-variable :after
    (client (environment env:run-time-environment) variable-name
     &optional (value nil valuep))
  (when valuep
    (let ((cell (env:ensure-variable-cell client environment variable-name)))
      (unless (sys:variable-cell-boundp client cell)
        (setf (sys:variable-cell-value client cell) value)))))

(defmethod env:make-parameter
    (client (environment env:run-time-environment) variable-name new)
  (ecase (env:variable-status client environment variable-name)
    ((nil)
     (setf (sys:variable-cell-value
            client (env:ensure-variable-cell client environment variable-name))
           new
           (sys:variable-status client environment variable-name)
           :special))
    ((:special)
     (let ((cell (env:ensure-variable-cell client environment variable-name)))
       (setf (sys:variable-cell-value client cell) new)))
    ((:constant)
     (error 'env:attempt-to-define-special-variable-for-existing-constant
            :name variable-name))
    ((:symbol-macro)
     (error 'env:attempt-to-define-special-variable-for-existing-symbol-macro
            :name variable-name))))

(defmethod env:make-constant (client environment variable-name new)
  (ecase (env:variable-status client environment variable-name)
    ((nil) ; undefined: this is a new constant
     (setf (sys:variable-status client environment variable-name)
           :constant
           (sys:variable-cell-value
            client (env:ensure-variable-cell client environment variable-name))
           new))
    ((:constant)
     (let ((old
             ;; The cell must be bound, as constants always are.
             (sys:variable-cell-value
              client (find-variable-cell client environment variable-name))))
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

(defun env:variable-macro-expander (client environment variable-name)
  (labels ((%variable-macro-expander (environment)
             (or (sys:variable-macro-expander client environment variable-name)
               ;; we already checked the status,
               ;; so we know SOMETHING defines this expander.
               (%variable-macro-expander (env:parent client environment)))))
    (ecase (env:variable-status client environment variable-name)
      ((nil :special :constant) nil)
      ((:symbol-macro) (%variable-macro-expander environment)))))

(defun %make-symbol-macro-expander (expansion)
  (lambda (form env)
    (declare (ignore form env))
    expansion))

(defmethod env:make-symbol-macro (client environment variable-name expansion)
  (ecase (env:variable-status client environment variable-name)
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

(defmethod env:symbol-plist (client environment symbol)
  (if (sys:symbol-plist-known-p client environment symbol)
      (sys:symbol-plist client environment symbol)
      (labels ((%plist (env)
                 (if (sys:symbol-plist-known-p client env symbol)
                     (values (sys:symbol-plist client env symbol) t)
                     (let ((parent (env:parent client env)))
                       (if parent
                           (%plist parent)
                           (values nil nil))))))
        (let ((parent (env:parent client environment)))
          (if parent
              (multiple-value-bind (plist knownp) (%plist parent)
                (if knownp
                    (setf (sys:symbol-plist client environment symbol)
                          ;; copy-list so that alterations
                          ;; don't appear in parent
                          (copy-list plist))
                    nil))
              nil)))))

(defmethod (setf env:symbol-plist)
    (new client (environment env:run-time-environment) symbol)
  (setf (sys:symbol-plist client environment symbol) new))

;;; Types and classes

(defun find-type-cell (client environment type-name)
  "Find a type cell if it exists, or return NIL. Internal."
  (or (sys:type-cell client environment type-name)
    (let ((parent (env:parent client environment)))
      (if parent
          (find-type-cell client parent type-name)
          nil))))

(defmethod env:ensure-type-cell (client environment type-name)
  (or (sys:type-cell client environment type-name)
    (let* ((parent (env:parent client environment))
           (parent-cell
             (if parent
                 (find-type-cell client parent type-name)
                 nil))
           (new-cell
             (sys:ensure-type-cell client environment type-name)))
      (when parent-cell
        ;; Copy in the old value.
        (when (sys:type-cell-boundp client parent-cell)
          (setf (sys:type-cell-value client new-cell)
                (sys:type-cell-value client parent-cell))))
      new-cell)))

(defmethod env:find-class (client environment class-name &optional (errorp t))
  (let ((cell (find-type-cell client environment class-name)))
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
          client (env:ensure-type-cell client environment class-name)))
        (t
         (setf (sys:type-cell-value
                client (env:ensure-type-cell client environment class-name))
               new)))
  new)

(defmethod env:type-expander (client environment type-name)
  (let ((cell (sys:type-cell client environment type-name)))
    (cond ((and cell (sys:type-cell-boundp client cell)) nil)
          ((sys:type-expander client environment type-name))
          (t (let ((parent (env:parent client environment)))
               (if parent
                   (env:type-expander client parent type-name)
                   nil))))))
(defmethod (setf env:type-expander) (expander client environment type-name)
  (setf (sys:type-expander client environment type-name) expander)
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

;;; Packages

(defmethod env:find-package (client (environment env:run-time-environment) name)
  (or (sys:find-package client environment name)
    (let ((parent (env:parent client environment)))
      (if parent
          (setf (sys:find-package client environment name)
                (env:find-package client parent name))
          nil))))
(defmethod (setf env:find-package)
    (new client (environment env:run-time-environment) name)
  (setf (sys:find-package client environment name) new))

(defmethod env:package-name (client (environment env:run-time-environment) package)
  (or (sys:package-name client environment package)
    (let ((parent (env:parent client environment)))
      (if parent
          (setf (sys:package-name client environment package)
                (env:package-name client parent package))
          nil))))
(defmethod (setf env:package-name)
    (new client (environment env:run-time-environment) package)
  (setf (sys:package-name client environment package) new))

(defmethod env:map-all-packages (client (env env:run-time-environment) function)
  (let ((parent (env:parent client env)))
    (if parent
        ;; In order to get everything including parents, we gather up a list and then
        ;; map over that. Kind of inefficient.
        (let ((packages nil))
          (declare (dynamic-extent packages))
          (sys:map-all-packages client env (lambda (p) (push p packages)))
          (env:map-all-packages client parent (lambda (p) (pushnew p packages)))
          (mapc function packages))
        ;; Easy case.
        (sys:map-all-packages client env function))))

(defmethod env:package-names (client (env env:run-time-environment) package)
  (append
   (sys:package-names client env package)
   (let ((parent (env:parent client env)))
     (if parent
         (env:package-names client parent package)
         nil))))

;;; Optimize

(defmethod env:optimize (client environment) (sys:optimize client environment))
(defmethod env:proclaim-optimize (client environment optimize)
  (setf (sys:optimize client environment)
        (env:merge-optimize client optimize (env:optimize client environment))))

;;; Combination

(defmethod env:macroexpand-1 (client environment (form symbol))
  (let ((expander (env:variable-macro-expander client environment form)))
    (if expander
        (values (funcall *macroexpand-hook* expander form environment) t)
        (values form nil))))

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
  (case (env:variable-status client environment form)
    ((:constant) t)
    ((:symbol-macro)
     (env:constantp
      client environment (env:macroexpand-1 client environment form)))
    (otherwise nil)))

(defmethod env:constantp (client environment (form cons))
  (declare (ignore client environment))
  ;; This method is kind of awkward: A client must take care of QUOTE itself
  ;; to use this function for cl:constantp. Clostrum does not assume that QUOTE
  ;; always has its standard meaning, and because it imposes no meaning on
  ;; special operator values (returned by fdefinition), it has no way to check
  ;; that QUOTE has its standard meaning.
  nil)

(defmethod env:constantp (client environment (form t))
  (declare (ignore client environment))
  ;; not a symbol or cons. therefore the form is self-evaluating.
  t)
