(in-package #:clostrum-implementation)

(documentation-utils:define-docs
  (function sys:operator-status
    "Return the status of OPERATOR-NAME's fbinding in ENVIRONMENT.
The status is either NIL, meaning not fbound, or :FUNCTION, :MACRO, or :SPECIAL-OPERATOR.")
  (function (setf sys:operator-status)
    "Set the status of OPERATOR-NAME's fbinding in ENVIRONMENT.
The status is either NIL, meaning not fbound, or :FUNCTION, :MACRO, or :SPECIAL-OPERATOR.")
  (function sys:operator-cell
    "Retrieve the cell for OPERATOR-NAME's fbinding in ENVIRONMENT.
The nature of the cell is implementation-defined, except that it must work with OPERATOR-CELL-VALUE, OPERATOR-CELL-BOUNDP, and OPERATOR-CELL-MAKUNBOUND.
Calls to this function always retrieve the same cell given the same arguments, regardless of the operator being fbound or not.")
  (function sys:compiler-macro-function
    "Return the compiler macro function for OPERATOR-NAME in ENVIRONMENT.
This is NIL if no function has been set, or else the object set by (SETF COMPILER-MACRO-FUNCTION).")
  (function (setf sys:compiler-macro-function)
    "Set the compiler macro function for OPERATOR-NAME in ENVIRONMENT.")
  (function sys:setf-expander
    "Return the setf expander for OPERATOR-NAME in ENVIRONMENT.
This is NIL if no expander has been set, or else the object set by (SETF SETF-EXPANDER).
The nature of a setf expander is otherwise implementation-defined. One choice would be to have it as a function of two arguments, a place and an environment, analogous to a macro expander, that returns the setf expansion.")
  (function (setf sys:setf-expander)
    "Set the setf expander for OPERATOR-NAME in ENVIRONMENT.")
  (function sys:operator-cell-value
    "Get the value stored in CELL. The CELL is an object returned by OPERATOR-CELL. If OPERATOR-CELL-BOUNDP is not true, a function signals UNDEFINED-FUNCTION when called with any arguments is returned.")
  (function (setf sys:operator-cell-value)
    "Set the value stored in CELL. The CELL is an object returned by OPERATOR-CELL.")
  (function sys:operator-cell-boundp
    "Return true iff CELL has a value. The CELL is an object returned by OPERATOR-CELL.")
  (function sys:operator-cell-makunbound
    "Make CELL have no value. The CELL is an object returned by OPERATOR-CELL.
The return values of this function are undefined."))

(documentation-utils:define-docs
  (function sys:variable-status
    "Return the status of VARIABLE-NAME's definition in ENVIRONMENT.
The status is either NIL, meaning not defined, or :SPECIAL, :CONSTANT, or :SYMBOL-MACRO.
Note that a status of NIL or :SYMBOL-MACRO does not preclude the variable having a value, a set by (SETF SYMBOL-VALUE).")
  (function (setf sys:variable-status)
    "Set the status of VARIABLE-NAME's definition in ENVIRONMENT.
Note that a status of NIL or :SYMBOL-MACRO does not preclude the variable having a value, a set by (SETF SYMBOL-VALUE).")
  (function sys:variable-cell
    "Retrieve the cell for VARIABLE-NAME's binding in ENVIRONMENT.
The nature of the cell is implementation-defined, except that it must work with OPERATOR-CELL-VALUE, OPERATOR-CELL-BOUNDP, and OPERATOR-CELL-MAKUNBOUND.
Calls to this function always retrieve the same cell given the same arguments, regardless of the operator being fbound or not.")
  (function sys:variable-macro-expander
    "Get the symbol macro expander for VARIABLE-NAME in ENVIRONMENT.
This is NIL if no expander has been set, or else the object set by (SETF VARIABLE-MACRO-EXPANDER).")
  (function (setf sys:variable-macro-expander)
    "Set the symbol macro expander for VARIABLE-NAME in ENVIRONMENT.")
  (function sys:variable-cell-value
    "Get the value stored in CELL. The CELL is an object returned by VARIABLE-CELL. If VARIABLE-CELL-BOUNDP is not true of the cell, the effects are undefined.")
  (function sys:variable-cell-boundp
    "Return true iff CELL has a value. The CELL is an object returned by VARIABLE-CELL.")
  (function sys:variable-cell-makunbound
    "Make CELL have no value. The CELL is an object returned by VARIABLE-CELL.
The return values of this function are undefined."))

(documentation-utils:define-docs
  (function sys:type-cell
    "Retrieve the cell for TYPE-NAME's class binding in ENVIRONMENT.
The nature of the cell is implementation-defined, except that it must work with TYPE-CELL-VALUE, TYPE-CELL-BOUNDP, and TYPE-CELL-MAKUNBOUND.
Calls to this function always retrieve the same cell given the same arguments, regardless of the operator being fbound or not.")
  (function sys:type-expander
    "Return the type expander for TYPE-NAME in ENVIRONMENT.
This is NIL if no expander has been set, or else the object set by (SETF TYPE-EXPANDER).
The nature of a type expander is otherwise implementation-defined. One choice would be to have it as a function of two arguments, a type specifier and an environment, analogous to a macro expander, that returns the expanded type specifier.")
  (function (setf sys:type-expander)
    "Set the type expander for OPERATOR-NAME in ENVIRONMENT.")
  (function sys:type-cell-value
    "Get the value stored in CELL. The CELL is an object returned by TYPE-CELL. If TYPE-CELL-BOUNDP is not true of the cell, the effects are undefined.")
  (function sys:type-cell-boundp
    "Return true iff CELL has a value. The CELL is an object returned by TYPE-CELL.")
  (function sys:type-cell-makunbound
        "Make CELL have no value. The CELL is an object returned by TYPE-CELL.
The return values of this function are undefined."))

(documentation-utils:define-docs
  (function sys:find-package
    "Find the package bound to NAME in ENVIRONMENT, or NIL if none has been defined.")
  (function (setf sys:find-package)
    "Set the package bound to NAME in ENVIRONMENT.")
  (function sys:proclamation
    "Find the proclamation associated with NAME in ENVIRONMENT. The nature of proclamations is client-defined.")
  (function (setf sys:proclamation)
    "Set the proclamation associated with NAME in ENVIRONMENT."))

(documentation-utils:define-docs
  (function sys:function-description
    "Get the compiler function description for FUNCTION-NAME in ENVIRONMENT.")
  (function (setf sys:function-description)
    "Set the compiler function description for FUNCTION-NAME in ENVIRONMENT.")
  (function sys:variable-description
    "Get the compiler variable description for VARIABLE-NAME in ENVIRONMENT.")
  (function (setf sys:variable-description)
    "Set the compiler variable description for VARIABLE-NAME in ENVIRONMENT.")
  (function sys:type-description
    "Get the compiler type description for TYPE-NAME in ENVIRONMENT.")
  (function (setf sys:type-description)
    "Set the compiler type description for TYPE-NAME in ENVIRONMENT."))

(documentation-utils:define-docs
  (function env:fdefinition
    "As CL:FDEFINITION. Get the function definition for OPERATOR-NAME in ENVIRONMENT. If OPERATOR-NAME is not fbound in ENVIRONMENT, signal an UNDEFINED-FUNCTION error.
If the operator names a macro, the object passed to (SETF MACRO-FUNCTION) will be returned. If the operator names a special operator, the object passed to MAKE-SPECIAL-OPERATOR will be returned.")
  (function (setf env:fdefinition)
    "As (SETF CL:FDEFINITION). Set the function definition for OPERATOR-NAME in ENVIRONMENT, and make it defined as a function (rather than a macro or special operator).")
  (function env:fboundp
    "As CL:FBOUNDP. Return true iff OPERATOR-NAME is fbound in ENVIRONMENT.")
  (function env:fmakunbound
    "As CL:FMAKUNBOUND. Make OPERATOR-NAME have no function value in ENVIRONMENT. Returns OPERATOR-NAME.")
  (function env:macro-function
    "As CL:MACRO-FUNCTION. Return the macro function for OPERATOR-NAME in ENVIRONMENT, or NIL if it does not name a macro.")
  (function (setf env:macro-function)
    "As (SETF CL:MACRO-FUNCTION). Set the macro function for OPERATOR-NAME in ENVIRONMENT, and make it defined as a macro.")
  (function env:special-operator-p
    "As CL:SPECIAL-OPERATOR-P. Return true iff OPERATOR-NAME names a special operator in ENVIRONMENT.")
  (function env:make-special-operator
    "Make OPERATOR-NAME a special operator in ENVIRONMENT. Future calls to FDEFINITION will return the object given as NEW.")
  (function sys:setf-expander
    "Return the setf expander for OPERATOR-NAME in ENVIRONMENT.
This is NIL if no expander has been set, or else the object set by (SETF SETF-EXPANDER).
The nature of a setf expander is otherwise implementation-defined. One choice would be to have it as a function of two arguments, a place and an environment, analogous to a macro expander, that returns the setf expansion.")
  (function (setf sys:setf-expander)
    "Set the setf expander for OPERATOR-NAME in ENVIRONMENT. OPERATOR-NAME must already be defined as a function or macro."))

(documentation-utils:define-docs
  (function env:symbol-value
    "As CL:SYMBOL-VALUE. Return the global value of VARIABLE-NAME in ENVIRONMENT.")
  (function (setf env:symbol-value)
    "As (SETF CL:SYMBOL-VALUE). Set the global value of VARIABLE-NAME in ENVIRONMENT.")
  (function env:boundp
    "As CL:BOUNDP. Return true iff VARIABLE-NAME has a global value in ENVIRONMENT.")
  (function env:makunbound
    "As CL:MAKUNBOUND. Make VARIABLE-NAME have no global value in ENVIRONMENT. Returns VARIABLE-NAME.")
  (function env:make-variable
    "Functional version of CL:DEFVAR. Proclaim VARIABLE-NAME special, and if it has no global value, set its global value to VALUE.")
  (function env:make-parameter
    "Functional version of CL:DEFPARAMETER. Proclaim VARiABLE-NAME special, and set its global value to VALUE.")
  (function env:make-constant
    "Functional version of CL:DEFCONTANT. Make VARIABLE-NAME constant and set its global value to VALUE.")
  (function env:make-symbol-macro
    "Functional version of CL:DEFINE-SYMBOL-MACRO. Make VARIABLE-NAME a symbol macro and set its expansion to EXPANSION."))

(documentation-utils:define-docs
  (function env:find-class
    "As CL:FIND-CLASS. Return the class named CLASS-NAME in ENVIRONMENT. If there is no such class, return NIL, unless ERRORP is true in which case an error of type ERROR is signaled.")
  (function (setf env:find-class)
    "As (SETF CL:FIND-CLASS). Set the class for CLASS-NAME in ENVIRONMENT. ERRORP is ignored.")
  (function env:make-type
    "Functional version of CL:DEFTYPE. Define TYPE-NAME to be a derived type in ENVIRONMENT, with type expander EXPANDER. The consequences are unspecified if TYPE-NAME already names a class.")
  (function env:type-expand-1
    "Operator analogous to CL:MACROEXPAND-1, but for type specifiers. Given a type specifier and an environment, return (values expansion true) if the type specifier is derived, otherwise (values specifier nil).")
  (function env:type-expand
    "Operator analogous to CL:MACROEXPAND, but for type specifiers. Given a type specifier and an environment, repeatedly expand the specifier as a derived type, and then return (values expansion true) if it was ever expanded, and otherwise (values specifier nil)."))

(documentation-utils:define-docs
  (function env:macroexpand-1
    "As CL:MACROEXPAND-1. Macroexpand FORM in ENVIRONMENT once, and return the expansion and true, or the form and NIL if it was not a macro form.")
  (function env:macroexpand
    "As CL:MACROEXPAND. Repeatedly macroexpand FORM in ENVIRONMENT, and return the expansion and true, or the form and NIL if it was not a macro form.")
  (function env:constantp
    "As CL:CONSTANTP. Return true if the FORM is constant in ENVIRONMENT.
Note that the default method on this function does _not_ check for CL:QUOTE forms, as CL:CONSTANTP must, because there is no way for Clostrum to know that CL:QUOTE has the same semantics in ENVIRONMENT."))
