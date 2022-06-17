Most Common Lisp implementations have a single global environment, and
it is spread out in various parts of the system.  For example, the
definition of a *named function* may be contained in a slot of the
symbol representing the name, whereas the definition of a *type* might
be in a hash table that is the value of a special variable.

In the paper "First-class Global Environments in Common Lisp",
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
