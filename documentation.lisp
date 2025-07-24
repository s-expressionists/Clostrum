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
If no cell yet exists in this environment, NIL is returned.

See ENSURE-OPERATOR-CELL")
  (function sys:ensure-operator-cell
    "Ensure that OPERATOR-NAME has a cell in ENVIRONMENT, and return that cell. Calls to this function always retrieve the same cell given the same arguments, regardless of the operator being fbound or not.")
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
  (function sys:operator-inline
    "Return the inline proclamation for the given operator. This is either CL:INLINE, CL:NOTINLINE, or NIL, meaning no proclamation either way.")
  (function (setf sys:operator-inline)
    "Set the inline proclamation for the given operator.")
  (function sys:operator-inline-known-p
    "Return true iff the inline proclamation for this operator has been set. This function exists so that the proclamation can be set to NIL in a child environment without the inheritance-aware high level API functions deciding to consult a parent instead.")
  (function sys:operator-inline-data
    "Return the inline data for the given operator. The nature of this data is defined by the client.")
  (function (setf sys:operator-inline-data)
    "Set the inline data for the given operator.")
  (function sys:operator-ftype
    "Return the proclaimed ftype for the given operator. Clostrum does not impose any kind of representation of types on clients, so using OPERATOR-FTYPE before an ftype is proclaimed (by (SETF OPERATOR-FTYPE)) may result in an error. Implementations are recommended to install a default type themselves to avoid this.")
  (function (setf sys:operator-ftype)
    "Proclaim an ftype for the given operator. Clostrum does not impose any kind of representation of types on clients.")
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
  (function sys:variable-type
    "Return the proclaimed type for the given variable. Clostrum does not impose any kind of representation of types on clients, so using VARIABLE-FTYPE before a type is proclaimed (by (SETF VARIABLE-TYPE)) may result in an error. Implementations are recommended to install a default type themselves to avoid this.")
  (function (setf sys:variable-type)
    "Proclaim a type for the given variable. Clostrum does not impose any kind of representation of types on clients.")
  (function sys:symbol-plist
    "Return the plist for a symbol.")
  (function (setf sys:symbol-plist)
    "Set the plist for a symbol.")
  (function sys:symbol-plist-known-p
    "Return true iff the symbol's plist in this environment has been set. This is used by the high-level API to disambiguate the situation in which a symbol plist has been set to NIL in one environment but set to something non-NIL in a parent.")
  (function sys:variable-cell-value
    "Get the value stored in CELL. The CELL is an object returned by VARIABLE-CELL. If VARIABLE-CELL-BOUNDP is not true of the cell, the effects are undefined.")
  (function sys:variable-cell-boundp
    "Return true iff CELL has a value. The CELL is an object returned by VARIABLE-CELL.")
  (function sys:variable-cell-makunbound
    "Make CELL have no value. The CELL is an object returned by VARIABLE-CELL.
The return values of this function are undefined.")
  (function sys:symbol-plist
    "Retrieve the plist attached to SYMBOL in ENVIRONMENT.")
  (function (setf sys:symbol-plist)
    "Set the plist attached to SYMBOL in ENVIRONMENT."))

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
    "Set the package bound to NAME in ENVIRONMENT. This can be used to define both nicknames and the proper name.")
  (function sys:package-names
    "Return a fresh list of all names of PACKAGE in ENVIRONMENT.")
  (function sys:package-name
    "Return the name of PACKAGE in ENVIRONMENT, or NIL if it has none.")
  (function (setf sys:package-name)
    "Set the name of PACKAGE in ENVIRONMENT. Note that this function does not necessarily establish the name for FIND-PACKAGE.")
  (function env:find-package
    "Find the package bound to NAME in ENVIRONMENT, or NIL if none has been defined.")
  (function sys:map-all-packages
    "Call FUNCTION on all PACKAGES in ENVIRONMENT, in some undefined order. This can be used for example to implement LIST-ALL-PACKAGES.")
  (function sys:proclamation
    "Find the proclamation associated with NAME in ENVIRONMENT. The nature of proclamations is client-defined. This mechanism is intended for implementing the DECLARATION declaration, or CLTL2's DEFINE-DECLARATION.")
  (function (setf sys:proclamation)
    "Set the proclamation associated with NAME in ENVIRONMENT.")
  (function sys:optimize
    "Return the OPTIMIZE proclamation data for ENVIRONMENT. The nature of this data is client-defined.
The default method expects OPTIMIZE proclamation data to be a list of optimize qualities or (quality value) lists, i.e. the CDR of an OPTIMIZE declaration specifier.")
  (function (setf sys:optimize)
    "Set the OPTIMIZE proclamation data for ENVIRONMENT. The nature of this data is client-defined.
The default method expects OPTIMIZE proclamation data to be a list of optimize qualities or (quality value) lists, i.e. the CDR of an OPTIMIZE declaration specifier.")
  (function env:optimize
    "Return the OPTIMIZE proclamation for ENVIRONMENT.")
  (function env:proclaim-optimize
    "Proclaim a new optimize proclamation for ENVIRONMENT. OPTIMIZE is merged with the existing proclamation by MERGE-OPTIMIZE, and this merged proclamation is stored in the environment.

See MERGE-OPTIMIZE")
  (function env:merge-optimize
    "Compute an optimize proclamation from two optimize proclamations. NEW-OPTIMIZE is the new data dn OLD-OPTIMIZE is the old data.
The default method assumes that optimize proclamation data is a list of qualities or (quality value) lists. It normalizes any quality symbols to (quality 3), and discards any qualities in OLD-OPTIMIZE that are present in NEW-OPTIMIZE. Nonstandard optimization qualities are discarded."))

(documentation-utils:define-docs
  (function sys:parent
    "Given an environment, return the environment it inherits from, or NIL if there is no such parent.")
  (type env:environment
    "Abstract parent class of all environments.")
  (type env:run-time-environment
    "Abstract class of run-time environments, containing actual definitions.")
  (type env:compilation-environment
    "Abstract class of compilation environments, containing information for the compiler to use."))

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
  (function env:note-function
    "Make OPERATOR-NAME a function in ENVIRONMENT, without providing a definition. The name will not be fbound, but this information can be retrieved by OPERATOR-STATUS. This function is provided to implement the (optional) compile time side effect of DEFUN.")
  (function sys:setf-expander
    "Return the setf expander for OPERATOR-NAME in ENVIRONMENT.
This is NIL if no expander has been set, or else the object set by (SETF SETF-EXPANDER).
The nature of a setf expander is otherwise implementation-defined. One choice would be to have it as a function of two arguments, a place and an environment, analogous to a macro expander, that returns the setf expansion.")
  (function (setf sys:setf-expander)
    "Set the setf expander for OPERATOR-NAME in ENVIRONMENT. OPERATOR-NAME must already be defined as a function or macro.")
  (function env:compiler-macro-function
    "As CL:COMPILER-MACRO-FUNCTION, return the compiler macro function associated with OPERATOR-NAME, or NIL if there isn't one.")
  (function (setf env:compiler-macro-function)
    "Set the compiler macro function associated with OPERATOR-NAME. If NEW is NIL, any previously associated compiler macro is removed."))

(documentation-utils:define-docs
  (function env:symbol-value
    "As CL:SYMBOL-VALUE. Return the global value of VARIABLE-NAME in ENVIRONMENT.")
  (function (setf env:symbol-value)
    "As (SETF CL:SYMBOL-VALUE). Set the global value of VARIABLE-NAME in ENVIRONMENT.")
  (function env:boundp
    "As CL:BOUNDP. Return true iff VARIABLE-NAME has a global value in ENVIRONMENT.")
  (function env:makunbound
    "As CL:MAKUNBOUND. Make VARIABLE-NAME have no global value in ENVIRONMENT. Returns VARIABLE-NAME.")
  (function env:symbol-plist
    "As CL:SYMBOL-PLIST.")
  (function (setf env:symbol-plist)
    "As (SETF CL:SYMBOL-PLIST).")
  (function env:make-variable
    "Functional version of CL:DEFVAR. Proclaim VARIABLE-NAME special, and if it has no global value and VALUE is provided, set its global value to VALUE.")
  (function env:make-parameter
    "Functional version of CL:DEFPARAMETER. Proclaim VARiABLE-NAME special, and set its global value to VALUE.")
  (function env:make-constant
    "Functional version of CL:DEFCONTANT. Make VARIABLE-NAME constant and set its global value to VALUE.")
  (function env:make-symbol-macro
    "Functional version of CL:DEFINE-SYMBOL-MACRO. Make VARIABLE-NAME a symbol macro and set its expansion to EXPANSION.")
  (function env:variable-macro-expander
    "Retrieve the symbol macro function associated with VARIABLE-NAME if there is one, or else return NIL. The variable macro expander is a function of two arguments, a form and an environment."))

(documentation-utils:define-docs
  (function env:find-class
    "As CL:FIND-CLASS. Return the class named CLASS-NAME in ENVIRONMENT. If there is no such class, return NIL, unless ERRORP is true in which case an error of type ERROR is signaled.")
  (function (setf env:find-class)
    "As (SETF CL:FIND-CLASS). Set the class for CLASS-NAME in ENVIRONMENT. ERRORP is ignored.")
  (function env:type-expand-1
    "Operator analogous to CL:MACROEXPAND-1, but for type specifiers. Given a type specifier and an environment, return (values expansion true) if the type specifier is derived, otherwise (values specifier nil).")
  (function env:type-expand
    "Operator analogous to CL:MACROEXPAND, but for type specifiers. Given a type specifier and an environment, repeatedly expand the specifier as a derived type, and then return (values expansion true) if it was ever expanded, and otherwise (values specifier nil).")
  (function env:merge-types
    "Given two types, compute their conjunction (i.e. the AND of their specifiers). Clients with custom type representations must specialize this method in order for type retrieval on environments with parents to work correctly.
The default method assumes that types are represented by their specifiers, and so simply forms the list (AND TYPE1 TYPE2)."))

(documentation-utils:define-docs
  (function env:macroexpand-1
    "As CL:MACROEXPAND-1. Macroexpand FORM in ENVIRONMENT once, and return the expansion and true, or the form and NIL if it was not a macro form.")
  (function env:macroexpand
    "As CL:MACROEXPAND. Repeatedly macroexpand FORM in ENVIRONMENT, and return the expansion and true, or the form and NIL if it was not a macro form.")
  (function env:constantp
    "As CL:CONSTANTP. Return true if the FORM is constant in ENVIRONMENT.
Note that the default method on this function does _not_ check for CL:QUOTE forms, as CL:CONSTANTP must, because there is no way for Clostrum to know that CL:QUOTE has the same semantics in ENVIRONMENT."))
