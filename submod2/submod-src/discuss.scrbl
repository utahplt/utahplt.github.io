#lang scribble/sigplan
@(require "utils.rkt")

@title{Implementation and Discussion}

We added submodules to Racket in version 5.3 (August 2012), which is
10 years after originally adding modules to Racket (then PLT
Scheme). The idea of nested scopes is obvious enough that the initial
implementation of modules was soon paired with a @racket[package]
macro for nested binding scope---imitating the nestable
@racket[module] form of Chez Scheme@~cite[Chez-modules]; nested
namespaces never found much use in the Racket code base,
however. Submodules, in contrast, have found immediate and widespread
use, solving many different problems that we did not originally
recognize as related: how to have code that is run when the module is
``main'' (in a more principled way that a Python-style dynamic test),
how to include test code with a library's implementation, how to
manage documentation in a library's implementation, how to provide new
exports from a module that are available only when specifically
requested, how to package a read-time parser for a new language
alongside its compile-time and run-time implementation, and how to
declare dynamic dependencies for a module for use by a packaging tool
that computes the dependencies without running the program.
Naturally, we take the fact that submodules are conceptually simple
but solve many problems as evidence that the submodule design is on
the right track.

The key idea in our design is to allow a nested namespace to have its
own dynamic extent relative to its enclosing environment. In a sense,
submodules are a ``meta'' form of closures: in the same way that
@racket[(define (f x) (lambda (y) x))] returns a function that has
access to the argument @racket[x] beyond the dynamic extent of a
call to @racket[f], submodules provide a way for a nested module to
refer to the bindings of an enclosing module without necessarily
implying a connection on the module extents (depending on how the
bindings are referenced). More generally, the design of modules,
phases, and submodules in Racket is predicated on the principle that
humans are good at reasoning with lexical scope, whether reasoning
about functions, about macro expansion, or about dependencies among
libraries and execution layers.

The design would look simpler if @racket[module] and the two
@racket[module*] variants (with and without an initial import) could
be collapsed into a single syntactic form. Indeed, it may be possible
to collapse @racket[module] and @racket[module*] and have the compiler
infer (based on later @racket[require]s) whether a submodule must be
compiled before or after the rest of the enclosing module's body.  The
difference between @racket[#f] or a module name in the initial-import
position of @racket[module*] might also be managed by cleaner
syntax. We leave these problems for future work, and as a practical
matter, choosing @racket[module] or @racket[module*] is easy enough
for a Racket programmer.

The Racket implementation of modules includes a primitive
@racket[for-label] form in addition to @racket[for-syntax] and
@racket[for-template]. A @racket[for-label] import corresponds to
binding at phase -∞: arbitrarily far in the future. The @racket[code]
form looks for bindings at this label phase for generating hyperlinks,
and in the expansion sketch of @figure-ref["srcdoc-expanded"], the
generated module's body has been shifted by -∞ (but that fact is
invisible in the sketch). The label phase is a kind of optimization
hint to the module system, where a @racket[for-label] dependency
implies that no execution of the module at any finite phase will
require the execution of the dependency.  The label phase seems
useful, but we are not yet sure whether it is fundamentally necessary.

As suggested by the model, top-level @racket[module] forms are the
unit of compilation in Racket. When a program is run from source in
Racket, then a module and all of its submodules must be compiled
together. In the case of a library module that contains its own
documentation, this compilation process involves much more code than
is needed to just run the library, negating an intended benefit of
submodules. Racket modules are normally compiled to bytecode in
advance, and the bytecode for a module starts with a directory of all
submodules separate from the main module code, so that the module or
any individual submodule can be loaded independently. Thus, a key
benefit of submodules in practice relies on bytecode compilation.
