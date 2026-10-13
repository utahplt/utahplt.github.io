#lang scribble/sigplan
@(require "utils.rkt")

@title{Introduction}

Racket's module system is similar to the module systems of other
languages. Modules are static, they reside in a global namespace with
hierarchical names, and they statically declare their dependencies on
other modules. These properties of the module system simplify
compilation and linking tasks; for example, the @exec{raco link} tool
can automatically traverse and compile the dependencies of a program
as needed, and @exec{raco exe} can combine all needed modules into a
single executable.

A distinguishing feature of Racket's module system is the way that it
interacts with macro-based ``languages.'' Each module explicitly
declares its language, so that different modules in the same program
can have different syntaxes or different semantics for a given
syntactic form. Such languages are implemented by macros via arbitrary
Racket code that runs at compile time, and the module-and-macro system
ensures that run-time and compile-time evaluation are kept
separate@~cite[macromod]. The separation of run time from compile time enables
compilation, analysis, and reasoning about programs in general, as
well as limiting evaluation at each phase to only the code that is
needed for that phase.

The original design for modules in Racket allows module declarations
only at the top level. Our new extension to Racket's module system
enables nested module declarations, which are called
@defterm{submodules}. In most module systems that support
nesting, lexically nested modules merely define a local namespace, and
they are instantiated along with their enclosing modules. Racket
submodules, in contrast, have a lifetime that can be independent of
the enclosing module. The code of a submodule need not even be loaded
when the enclosing module is used, or vice versa---unless the
submodule imports from the enclosing module, or vice versa, in which
case the usual phase-sensitive module loading and instantiation rules
apply.

Submodules solve a number of practical problems for Racket
programmers. They provide a natural way to express a ``main'' routine
that is used only when the module is run as a program and ignored when
the module is used as a library. Submodules provide a place for
testing internal functions without making the functions public and
without causing the tests to run on all uses of the module. Submodules
enable abstraction over sets of modules, which is not possible when
modules and only modules exist at the top level. Submodules also
provide a communication channel for static semantic information about
a module, such as whether the module is implemented in Typed
Racket@~cite[typed-racket] and the types of its exports.

More generally, submodules give a programmer the ability to
define new phases along the same lines as the built-in run-time
and compile-time phases. We can think of tests, for example, as
``test-time'' code, as opposed to run-time code. Unlike the
distinction between the compile and run phases, the test phase
subsumes the run phase, but not vice versa. As another example, when
documentation is written as part of the library that it documents,
then the code to produce the documentation is document-time code,
which is independent of run-time code. In much the same way that
run-time code and compile-time code can coexist within a module and
benefit from a shared lexical scope (so that the macro's expansion can
conveniently refer to bindings in the same module), document-time code
can coexist with run-time code and benefit from a shared scope (so
that the documentation can conveniently refer to bindings in the
module, where the references are be rendered as links to the bindings'
documentation).

In this paper, we describe Racket's module system, its handling of
different phases, and its support for submodules. We show how
submodules are used for tasks such as testing and
documentation. Finally, we provide a formal model that specifies the
interaction of macro expansion, module declarations, compilation, and
evaluation.
