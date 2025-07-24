Most Common Lisp implementations have a single global environment, and
it is spread out in various parts of the system.  For example, the
definition of a *named function* may be contained in a slot of the
symbol representing the name, whereas the definition of a *type* might
be in a hash table that is the value of a special variable.

In the paper [First-class Global Environments in Common
Lisp](http://metamodular.com/SICL/environments.pdf),
presented at the European Lisp Symposium in 2015, we outlined a
protocol for an alternative way of representing global environments,
namely as first-class instances of a standard classes.  This library
is a concrete implementation of the ideas in that paper, though
improved upon since.

We provide protocols and default implementations for the three kinds
of global environments mentioned in the Common Lisp standard, namely
the *run-time* environment, the *evaluation* environment and the
*compilation* environment.

The use of a *client* parameter for our protocol generic functions
allows client code to customize the functions defined here, either by
extending them or by overriding them.  Similarly, the classes defined
here are designed to be possible to use as superclasses of
client-specific environment classes.

# API

The Clostrum API is divided into two parts, a "high level" package `clostrum`, and a "low level" package `clostrum-sys`. `clostrum-sys` has generic functions that access environments directly and simply, without error checking, while `clostrum`'s generic functions mimic standard Common Lisp operators' more complex behavior and account for inheritance. `clostrum` has default methods implemented in terms of `clostrum-sys`, so environment implementation need only specialize the `clostrum-sys` functions in order to make `clostrum`'s high level facilities available. Users (rather than implementors) of the Clostrum protocol should only need to use the `clostrum` package.

## High level interface

For operating on run-time environments, the `clostrum` package makes available environment manipulation functions and accessors similar to those in the standard: `fdefinition`, `fboundp`, `fmakunbound`, `macro-function`, `special-operator-p` for functions; `symbol-value`, `boundp`, `makunbound`, `symbol-plist` for variables; `find-class` for classes; `find-package` for packages; and `macroexpand-1`, `macroexpand`, and `constantp`.

There are additional functions and accessors corresponding to some other Lisp operators. `setf-expander` can be used for `define-setf-expander`; `make-variable`, `make-parameter`, `make-constant`, and `make-symbol-macro` for `defvar`, `defparameter`, `defconstant`, and `define-symbol-macro` respectively; `type-expander` for `deftype`, along with `type-expand-1` and `type-expand` to work with type specifiers.

For proclamations, `proclamation` can be used for `declaration` or CLtL2's `define-declaration`. `variable-type` covers `type`, and `special` proclamations can be done with `make-variable`. `operator-ftype` handles `ftype` and `operator-inline` `inline`. User-defined inlining data, such as a definition, can be associated with an operator through `operator-inline-data`.

`note-function` will declare an operator to be a function without needing a definition for said function. This is useful for the compile-time effect of `defun`.

The package environment can be accessed via `find-package`. New nicknames for a package can be installed via `(setf find-package)`, and the list of all names for a package in an environment retrieved via `package-names`. All packages in an environment can be iterated over with `map-all-packages`. Additionally, the canonical name of a package is considered part of the environment as well, and can be accessed via `package-name`.

### Compilation environments

Compilation environments support a subset of the runtime environment operations. They lack package mappings, and functions only needed at runtime like `(setf symbol-value)` are not implemented. `macro-function` and `make-constant` still work.

## Cells

Each variable, operator, and type has a _cell_ retrievable by name, and variables and operators have a _status_. The cell is an object of implementation-defined nature, accessed by `-value`, `-boundp`, and `-makunbound` functions, that holds the value of a binding. Cells are an essential part of Clostrum's design and explained more in the paper. The status indicates what kind of binding a variable or operator has, e.g. as a variable or a constant, or a function or a macro. Some statuses can be read but not directly written (in the high-level API).

In the high level API, cells are retrieved by `ensure-variable-cell`, etc. This will return a cell if it exists or make a new one to return if not. Undefining a variable etc. does _not_ remove the cell, so they can be used for continued access regardless of boundedness.

## Low-level API

The `clostrum-sys` package contains the functions that must be implemented by environment objects for the high level functions to work. These consist of direct access to cells and statuses, along with additional functions for the non-cell properties such as a compiler macro functions. Additionally, a `parent` function should be supported for inheritance, but the rest of the functions do not need to consult parent environments as this is handled by the high-level API.

# Inheritance semantics

The Common Lisp standard mentions environments inheriting from each other in the context of compilation (CLHS 3.2.1) but does not describe the semantics in detail. Clostrum supports inherited environments, and the parent of an environment can be read by `parent`; if an environment has no parent, `parent` returns `nil`.

The basic rule of inherited bindings Clostrum has adopted is as follows: The _values_ in bindings are inherited from parent environments, but the bindings _themselves_ are not. Mutating a binding or making it unbound has no effect on a parent environment; in fact, no operation on a child environment will mutate anything in the parent environment.

Since the standard only contemplates environment inheritance in the context of compilation, these inheritance semantics are designed to allow `compile-file` operations to mutate an evaluation environment without mutating anything in the parent. For example, a function definition or redefinition within `(eval-when (:compile-toplevel) ...)` can affect an evaluation environment, but after `compile-file` returns, this evaluation environment is discarded, and the startup environment leaves no trace.

# Basic

The `clostrum-basic` system is the reference implementation of Clostrum. It implements the low level interface, and so the high level interface can be used with the basic environments.

# Trucler

The `clostrum-trucler` system implements the [Trucler](https://github.com/s-expressionists/Trucler/) protocol for Clostrum environments. The system package has no exports, instead just specializing the Trucler protocol methods.
