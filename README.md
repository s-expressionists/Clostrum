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

The Clostrum API is divided into two parts, a "high level" package `clostrum`, and a "low level" package `clostrum-sys`. `clostrum-sys` has generic functions that access environments directly and simply, without error checking, while `clostrum`'s generic functions mimic standard Common Lisp operators' more complex behavior. `clostrum` has default methods implemented in terms of `clostrum-sys`, so environment implementation need only specialize the `clostrum-sys` functions in order to make `clostrum`'s high level facilities available.

## High level interface

The `clostrum` package makes available environment manipulation functions and accessors similar to those in the standard: `fdefinition`, `fboundp`, `fmakunbound`, `macro-function`, `special-operator-p` for functions; `symbol-value`, `boundp`, `makunbound` for variables; `find-class` for classes; `find-package` for packages; and `macroexpand-1`, `macroexpand`, and `constantp`.

There are additional functions and accessors corresponding to some other Lisp operators. `setf-expander` can be used for `define-setf-expander`; `make-variable`, `make-parameter`, `make-constant`, and `make-symbol-macro` for `defvar`, `defparameter`, `defconstant`, and `deine-symbol-macro` respectively; `proclamation` can be ued for the `declaration` proclamation or CLtL2's `define-declaration`; and `make-type` and `type-expander` for `deftype`, along with `type-expand-1` and `type-expand` to work with type specifiers.

### Compilation environments

The compilation environment interface is comparatively simple: `function-description`, `variable-description`, and `type-description` access named data in an environment. The nature of these compilation descriptions is left undefined by Clostrum, but using [Trucler](https://github.com/s-expressionists/Trucler) would be appropriate.

## Low level interface

In the low level interface, each variable, operator, and type has a _cell_ retrievable by name, and variables and operators have a _status_. The cell is an object of implementation-defined nature accessed by `-value`, `-boundp`, and `-makunbound` function that holds the value of a binding. Cells are an essential part of Clostrum's design and explained more in the paper. The status indicates what kind of binding a variable or operator has, e.g. as a variable or a constant, or a function or a macro.

There are some additional interfaces for values outside the cells, such as setf and type expanders.

# Basic

The `clostrum-basic` system is the reference implementation of Clostrum. It implements the low level interface, so the high level interface can be used with the basic environments. The classes `run-time-environment` and `compilation-environment` represent run-time and compilation environments respectively.
