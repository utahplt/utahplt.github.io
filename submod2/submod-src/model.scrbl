#lang scribble/sigplan
@(require "utils.rkt"
          "model-pict.rkt")

@title[#:tag "model"]{Model}

Our model of submodules in Racket shows how modules are compiled and
instantiated, including support for submodules, macros, and macros
that expand to submodule declarations. The model is implemented in PLT
Redex@~cite[redex-book], and while we cover only the most relevant
details here, the full model is available in executable and typeset
forms at

@centerline{@url{http://www.cs.utah.edu/plt/submod2/}}

@; ----------------------------------------

@section{Running Modules}

@figure[
"module-grammar"
"Compiled-module representation"
@(module-grammar-pict)
]

The representation of a compiled module, @nt{mod}, is shown in
@figure-ref["module-grammar"]. A @nt{mod} contains the module's name,
a set of modules that the module depends on, and a sequence of
definitions for the module's body.  Each dependency and definition is
associated with a particular phase, @nt{ph}. 
A normal @racket[require] of a module creates
a dependency with phase 0, while a @racket[require] with
@racket[for-syntax] creates a dependency with phase 1, and a
@racket[require] with @racket[for-template] creates a dependency with
phase -1.

Each definition is either for a run-time value or a macro, as
indicated by @nt{kind}, and its associated phase. A value definition
is evaluated at its associated phase and can be referenced in its
associated phase.  A macro definition at phase @nt{ph} can be
referenced from a phase @nt{ph} context, although the macro itself
runs at phase @tm[(p+ ph 1)].

@figure[
"run-mf"
@list{The @mf{run} metafunction}
@(run-pict)
]

A module is instantiated and run with the @mf{run} metafunction, as
shown in @figure-ref["run-mf"]. A store @tm[Σ] is updated with module
instantiations and returned by @mf{run}. The @mf{run} metafunction
evaluates a module body, but only after folding @mf{run} across all
dependencies of the module. For each dependency, if a module was
imported @racket[for-syntax], then its definitions will be shifted
higher by one phase, and if a module was imported
@racket[for-template], then its definitions will be shifted lower by
one phase.

A @nt{mod} does not record the original module's submodules, if
any. Instead, the module repository @nt{M} contains all compiled
module definitions, including those that were submodules, in a flat
namespace. For simplicity, the model assumes that all modules and
submodules in a program have globally distinct names. Any
module--submodule or not--that is not a dependency of the initially
run module will itself not be run.

@; ----------------------------------------

@section{Compiling Modules}

The @nt{M} module repository used by @mf{run} must be generated from a
sequence of module declarations, except for the predefined module
@racket[base]. A module declaration starts as an S-expression, @nt{s-exp},
which is either a name@footnote{We use the term @defterm{name} instead
of @defterm{symbol} for syntactic names. We use @defterm{symbol} for
the run-time reflection of such names, which always appear in our
model with the @ast{Quote} constructor. See @citet[(in-bib expmodel
", section 3.2.1")] for further discussion.}  or a parenthesized
sequence of S-expressions, as shown in @figure-ref["binding"].
The compiler must take an S-expression representation of a module,
such as
@;
@scodeblock{(module m base (define x (quote ok)))}
@;
and turn it into an executable form, @nt{mod}.

@figure[
"binding"
@list{Syntax objects and bindings}
@(s-exp-pict)
]

The entry point for compilation is @mf{cmodule}, which takes an
S-expression for a single module along with a repository of previously
compiled modules and returns an updated repository:
@;
@centerline{@(cmodule-pict)}
@;
Compilation of a module begins by @defterm{visiting} the module for the
new module's initial import. The @mf{visit} metafunction is just like
@mf{run}, but instead of running only phase 0 value definitions,
it runs all definitions for the given module in phase 1 or higher, as
well as all macro definitions at phase 0. The @mf{visit} metafunction
is called with an empty store, which reflects that every module
compilation starts with fresh state.

Meanwhile, the @mf{import} metafunction extracts a set of bindings
from the initial import, as shown in @figure-ref["binding"]; the model
omits @racket[provide], and instead assumes that all definitions of a
module are exported. The collected bindings are then applied to the
body of the module to be compiled via @mf{wrap}, which converts the
module's body from S-expressions to syntax objects, @nt{stx}. In
general, syntax objects enable lexically scoped macros@~cite[expmodel
syntax-case], but we simplify here by considering only model-level
scope.

Finally, each syntax object for a module's body is paired with the
phase at which it appears, producing a @nt{body} element of the form
@tm[(At stx ph)]. Initially, all body forms are at phase 0, but
@racket[begin-for-syntax] may later move forms to a higher phase.
This phase shifting is implemented in the @mf{cbody} metafunction,
which completes compilation of the module.
@;
@centerline{@(cbody-signature-pict)}
@;
In addition to the @nt{body} sequence, @mf{cbody} receives the name
@tm[mnam] of the module being compiled (which is eventually used to
create a @nt{mod} and add it to @nt{M}), a sequence of dependencies
that is extended by @racket[require] forms among the @nt{body}s,
a sequence of compiled @nt{defn}s that is extended by @racket[define]
and @racket[define-syntax] forms among the @nt{body}s, the set of
previously compiled modules @nt{M} that is extended by submodule
declarations among the @nt{body}s, and the current store @tm[Σ].

@figure[
"module-syntax"
@list{Module syntax recognized by @mf{cbody}}
@(module-language-pict)
]

The simplest case of @mf{cbody} is when no more @nt{body}s are left,
in which case the accumulated dependencies and definitions are
combined into a @nt{mod} and added to the result @nt{M}:
@;
@centerline{@(cbody-done-pict)}
@;
Otherwise, @mf{cbody} dispatches on the shape of the first @nt{body},
matching one of the @nt{form} cases of @figure-ref["module-syntax"].
The grammar of @nt{form} shows @racket[begin-for-syntax] as its first
case. More precisely, from the perspective of @mf{cbody}, a @nt{body}
to match that case must have a syntax object containing an identifier
whose binding is @racket[begin-for-syntax] from the pre-defined @racket[base]
module. That is, the forms listed in the grammar for @nt{form} are
available only when @racket[base] is imported or via macro expansion from
a module that imports @racket[base]. The last @nt{form} production is
a macro invocation, where macro expansion can produce any of the
other forms (or another macro invocation).

The @mf{cbody} rule for @racket[begin-for-syntax] is thus
@;
@centerline{@(cbody-begin-syntax-pict)}
@;
The syntax objects within @racket[begin-for-syntax] get shifted up
by one phase and added back to the list of bodies for the module.

We omit the rules for @racket[define], @racket[define-syntax], and
macro invocation. For the purpose of explaining submodules, an
interesting aspect of the omitted rules is that definitions are
evaluated only when they have a phase greater than 0, so that they can
be used in macro implementations, while definitions at phase 0 are
merely compiled.

@; -------- OMITTED: definitions & macro applications --------
@;{

The rule for @racket[define] is similarly triggered when the first syntax
object has the form @tm[(id_def id_new stx)] where @tm[id_def]
resolves to @racket[define] from @racket[base]:
@;
@centerline{@(cbody-define-pict)}
@;
The @mf{cexpr} metafunction (not shown here) compiles the right-hand side
of the definition in the current phase, given an initially empty local
environment in addition to the store @tm[Σ] (which may be consulted
for macro expansion); note that if the @racket[define] form was wrapped by
@racket[begin-for-syntax], then the phase has been incremented to 1, so
that @mf{cexpr} compiles the expression in phase 1. The extracted
@tm[dnam] and compiled @tm[ast] are then combined into @tm[defn_new],
which is accumulated into the set of definitions for the module.  The
phase, module name, and @tm[dnam] are also collected into a new
binding, which is applied to the remainder of the module body; for
simplicity, the model implements @racket[let*]-style binding for
module bodies.@footnote{Implementing recursive bindings would be
straightforward using an indirection and a store.} Finally, the new
definition may be evaluated; the @mf{maybe-eval} metafunction (not shown
here) evaluates its argument only if the current phase is greater than
0, otherwise it returns an arbitrary ``not evaluated, yet'' value.
If the definition is at phase 1 or higher (and therefore might be
used by macros later in the module), the result of evaluation
is added to the store before processing of the module body continues.

The rule for @racket[define-syntax] is mostly the same as for
@racket[define]:
@;
@centerline{@(cbody-define-syntax-pict)}
@;
The differences are that the right-hand side is compiled as an
expression at phase @tm[(p+ ph 1)], the new definition has kind
@tm[Macro], and it is always evaluated and added to the store for
use later in the module body.

To see a macro in action, we jump to the last production of @nt{form}
and the macro-invocation case of @mf{cbody}, which is triggered when
the leading identifier of the first syntax object has a binding whose
value in the store is a macro:
@;
@centerline{@(cbody-macro-pict)}
@;
When the macro binding is a @nt{func} (i.e., a @ast{Lambda} term),
then it is applied to the syntax object that represents the
macro invocation. The syntax object is wrapped with a @ast{Syntax}
constructor to reflect it as a value in an @nt{ast}. The
function call is constructed with an @ast{App} @nt{ast}, evaluated,
and the result must be a syntax object that is used in place of the
macro invocation as compilation of the module body continues.

}
@; ----------------------------------------

The @racket[require] case of @mf{cbody} uses a @mf{parse-req} metafunction (not shown)
to turn nested @racket[for-syntax] and @racket[for-template] specifications
into a phase shift:
@;
@centerline{@(cbody-require-pict)}
@;
Having extracted a module name @tm[mnam_in] and relative phase
@tm[ph_Δ], the @racket[require] rule of @mf{cbody} is essentially the
same as @mf{cmodule}: the imported module is visited, its bindings
are added to the rest of the module, and the module is recorded
as a dependency.

@figure[
"cbody-submodule"
@list{Submodule cases of @mf{cbody}}
@centerline{@(cbody-module-pict)}
]

The remaining cases of @mf{cbody} handle submodules, as shown in
@figure-ref["cbody-submodule"]. The @racket[module] rule and first
@racket[module*] rule are similar: the submodule is compiled using
@mf{cmodule}, and the difference is only whether the submodule is
compiled before or after the rest of the body of the current module. A
@racket[module] form is compiled before the rest of the body, so that it
can be used by a later @racket[require] in the current module. A
@racket[module*] form is compiled after the rest of the current module's
body, so that the submodule can @racket[require] the current module.

The last rule in @figure-ref["cbody-submodule"] handles
@racket[(module* @#,tm[mnam] () @#,nt{form} @#,elem{...})],
which represents a submodule that inherits all
bindings of its enclosing module. (We use @tm[()] instead of @tm[#f]
in the model to avoid the need for boolean literals in S-expressions
and syntax objects.) Inheriting all bindings of the enclosing module
is different from importing the enclosing module, because it makes any
imports of the enclosing module visible in the submodule, as opposed
to only the definitions of the enclosing module. Therefore, instead of
compiling the submodule via a context-stripping @mf{cmodule}, the
submodule's compilation uses @mf{cbody} directly. At the same time,
the bindings inherited by the submodule must be shifted by a negative
amount that corresponds to the submodule's phase nesting within its
enclosing module. Although the submodule inherits bindings of the
enclosing module, it does not inherit the store; the submodule's
compilation via @mf{cbody} starts with a fresh store that is
initialized by visiting the enclosing module at the appropriate
phase offset.

As noted for the first @mf{cbody} rule, the result of @mf{cbody} is
ultimately a set of compiled modules to act as the result of
@mf{cmodule}. Folding @mf{cmodule} over a set of S-expressions that
represent a program accumulates a set of compiled modules, which then
can be passed to @mf{run} with a main-module name to run the
program. The full model on the web site includes several example
programs as tests.
