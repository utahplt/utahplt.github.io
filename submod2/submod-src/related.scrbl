#lang scribble/sigplan
@(require "utils.rkt")

@title{Related Work}

By enabling programming at different layers,
submodules play a role similar to Java
annotations and C# attributes. JavaDoc, in particular, is a use of
Java annotations in the same way that Racket uses submodules for
in-source documentation. Java annotations allow the decoration of code
with data, and they are preserved through run time, so that
annotations can be inspected in source, compiled code or
reflectively. Still, Java annotations are limited to data, so that any
abstraction or programmatic interpretation of the data depends on yet
another external tool and language, or else the code part (such as
test to run for a @tt|{@Test}| annotation) is difficult to separate
from the main program. C# attributes can have associated methods, but
attribute code is still mingled with run-time code. Submodules, in
contrast, generalize annotations to make them ``live,'' so that the
language of annotations can include expressions, function, and even
syntactic extension, without necessarily tangling the submodule code
with the base code. At the same time, submodules allow these live
annotations to connect with the lexical scope of the associated code,
which is useful in cases such as testing and documentation.

Although the model of submodules in this paper uses a simplistic
notion of scope for syntax objects, in practice and in spirit
submodules build on a long line of work on hygienic
macros@~cite[hygiene syntax-case expmodel]. The @|R6RS| standard for
Scheme@~cite[R6RS-standard] also includes modules with phases, as
directly influenced by our previous work, but @|R6RS| does not include
submodules or @racket[begin-for-syntax], and it does not allow macro
expansion to introduce new imports within a module. @|R6RS| also does
not require implementations to enforce a phase distinction. Some
implementations support implicit phasing@~cite[implicit-phasing],
where @racket[for-syntax] and @racket[for-template] annotation on
imports are inferred automatically based on the use of imported
identifiers. In
Racket, we stick with explicit phasing because it supports different
bindings for the same name in different phases (which is useful, for
example, when documenting the @racket[define] form of some language in
a document that is implemented with Racket's normal @racket[define])
and because explicit phasing allows macro expansion to depend in a
reasonable way on side effects (as a last resort, but a useful one).
We also suspect that checking programmer expectations against
actual references is more useful in our context than implicit phasing's
inference.

In TemplateHaskell@~cite[TemplateHaskell], macro implementations are
restricted to pure functions, which perhaps lessens the need to track
phases; there is no question, for example, of initializing a GUI
subsystem as part of a macro expansion, and in the absence of
side-effects as side channels, the code that is accessed at compile
time can be more easily limited to that needed by the macro's
implementation. Since functions used by macros can be implemented
themselves with macros, TemplateHaskell effectively supports arbitrary
phase levels, the phases are implicit, and bindings are the same
across all phases. TemplateHaskell does not particularly support
phases other than the implicit phases of run and compile times, and
in-source documentation with Haddock@~cite[Haddock] is analogous to
JavaDoc. The Converge programming language@~cite[Converge] infers
phases for macro expansion in a similar way to TemplateHaskell.

The SugarJ@~cite[SugarJ] design for library-based language
extensibility relies on a separation between parsing code and
transformation code, while allowing the two parts to coexist in a
library. In SugarJ, the languages of parsing and transformation are
separate and distinct from the base language. Nevertheless, there is
room for a scope connection that is currently absent in SugarJ, which
would connect the output of the parser to specific transformations or
binding references in transformations to particular base-language
bindings.

Lightweight Module Staging@~cite[LMS Rompf-dissertation], or LMS, is
an approach to code generation where the type system is used to
separate code at different levels. While the details of LMS are
different from Racket modules and phases---particularly the way that
run-time values can be implicitly coerced to representations of
values---LMS is effective for fundamentally the same reason as modules
and phases: both rely on scope and binding to separate phases, and
both allow phases to lexically overlap without destroying a
programmer's ability to reason about the code.

More generally, multi-stage programming languages such as
MetaML@~cite[MetaML] include a notion of phases that is similar to the
notion in Racket. Racket's tracking of phases differs in that a syntax
object can accumulate bindings in many phases, and the determination
of the relevant binding can be made late, when the identifier is used
in an expression position. Staged languages more typically identify
the phase of each expression and identifier
statically. MetaML and other statically typed multi-stage languages
not only to ensure that an identifier is
used at a suitable phase, but they ensure that the identifier is used in
a suitable type context.

Nested modules in languages like Python, Chez
Scheme@~cite[Chez-modules], or ML (OCaml, SML/NJ) are
namespace-management tools, where nested modules are always
instantiated with the enclosing module. Racket submodules, in
contrast, are separately loadable and separately instantiable in the
same way that top-level modules are separately loadable and
instantiable.

Java classes are related to submodules in that the declaration of two
classes within a single compilation unit does not imply that both
classes must be used together at run time. Instead, classes are loaded
and initialized on demand in a Java implementation, which allows a
Java implementation to avoid loading code that is unused, and it
ensures dynamically that any code that is needed is loaded before it
is used.  With a macro-extensible Java in the style of
Maya@~cite[Maya], code that is used only at compile time will
naturally not be loaded at run time. This approach has the same
benefits and drawbacks as implicit scoping in Scheme (e.g., the
difficulty of reasoning about initialization side effects).

In this paper, we have used in-source documentation as one motivation
for submodules. We reported on in-source documentation for Racket in
previous work@~cite[scribble], but that report glosses over certain
problems. Documentation was extracted via @racket[include-extracted]
by re-expanding the source module and pulling designated syntax
objects out of the expansion. Besides being ugly and reporting syntax
errors late, the implementation had quadratic complexity for extracting
@math{N} sets of documentation from a single module. Our revised
implementation of in-source documentation uses a submodule to avoid
these problems.
