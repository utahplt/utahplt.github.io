#lang scribble/sigplan
@(require "utils.rkt"
          "repl-picts.rkt")

@title{Modules and Phases}

Before introducing submodules, we first explain Racket's design for
modules and phases with a recap of @citet[macromod].  In particular,
we motivate Racket's module system in terms of compilation, but we
mean ``compilation'' in a broad sense---as a proxy for any task that
requires understanding what a program means without actually running
the program. In a macro-extensible language, any such compilation
involves running code for macro expansion.

In a traditional Lisp or Scheme setting, a program is constructed by
loading code into a @racket[read]-@racket[eval]-@racket[print] loop
(REPL). For example, if a program is implemented across four
files, then it is linked together by having files @racket[load] other
files:@footnote{We use a @filepath{.scm} suffix in the figure because
many pre-@|R6RS| Scheme implementations often worked as illustrated.}
@;
@centerline{@(lists-slide #:show-deps? #f #:show-flat? #f)}
@;
In this example, note that the @filepath{top-10.scm} file uses
@racket[fold] without explicitly @racket[load]ing
@filepath{list.scm}. The program works, anyway, because the files are
effectively flattened into a linear sequence as they are loaded, and
@filepath{list.scm} gets loaded via @filepath{grocery.scm}, which is
in turn loaded before @filepath{top-10.scm}.

Relying on load-order side effects for program structuring is clearly
a bad idea. Reversing the initial loads of @filepath{top-10.scm} and
@filepath{grocery.scm} will break the program, even though a
programmer may be tempted to think of the files as modules that can be
imported in either order. Indeed, the lack of an explicit dependency
for @filepath{top-10.scm} on @filepath{list.scm} is likely to be a
mistake that will be discovered unfortunately late. As the number of
implementors and ``module'' files grows, the problem becomes
acute.

A step in the right direction is to introduce a concept of
``packages'' and constrain access to a variable to those in packages that
are explicitly imported with, say, a @racket[use] declaration:@footnote{We
use a  @filepath{.lsp} suffix in the figure because the Common Lisp
package system works roughly as illustrated.}
@;
@centerline{@(lists-slide #:add-load? #t #:show-deps? #f #:use? #t)}
@;
This hypothetical @racket[use] declaration plays two roles: it makes
the target package's bindings available in the current package, and it
ensures that the target package is loaded before the current package. In
other words, it helps avoid the mistakes possible with @racket[load] by
enforcing a connection between the lexical structure of the program and
it dynamic evaluation; the @racket[use] form leverages lexical scope.

Unfortunately, in a macro-extensible language, simply declaring
dependencies on bindings does not cover the full dependency story.
Suppose that a @filepath{kitchen.lsp} package depends on a
@filepath{grocery.lsp} package to supply both a function @racket[shop]
and a macro @racket[groceries] (to simplify the construction of
grocery lists), while @filepath{grocery.lsp} depends on @filepath{gui.lsp}
to implement a graphical list manager:
@;
@centerline{@(eval-when-slide #:macro? #t #:gui? #t)}
@;
On the one hand, if @racket[use] means only that
@filepath{grocery.lsp} must be loaded before @filepath{kitchen.lsp} is
run, then the implementation of the @racket[groceries] macro will not
be available at compile time for @filepath{kitchen.lsp}, which is when its
macros must be expanded. On the other hand, if @racket[use] means that
a package is loaded at both run and compile times, then compiling
@filepath{kitchen.lsp} means that @filepath{gui.lsp} will be loaded to
start a graphical interface even at compile time, but the graphical
interface should start only at run time.

The traditional solution to this problem is to annotate @racket[use]
declarations with an @racket[eval-when] declaration to say when the
corresponding package should be loaded. Such annotations, however, are
a return to @racket[load]-style scripting of dependencies, which are
difficult to get right when many packages are involved. Furthermore,
compile-time code must be split into separate packages from run-time
code, so that the packages can be loaded at different times.

Our solution to this problem in Racket is to again leverage lexical
scope. Racket modules with @racket[require] look
essentially the same as our hypothetical packages with @racket[use]:
@;
@centerline{@(eval-when-slide #:macro? #t #:gui? #t #:require? #t)}
@;
The meaning of @racket[require], however, is to run the
compile-time portions of the imported modules at compile time, and to
run the run-time portion of the imported modules at run-time. Thus, the
compile-time implementation of @racket[groceries] is available to
expand the body of @filepath{kitchen.rkt}, while @racket[shop],
@racket[list-editor-gui], and @racket[init-gui-application!] are
deferred until run time.

This definition of @racket[require] relies on the ability to
statically determine which parts of a module are for compile time and
which parts are for run time. To a first approximation, macro definitions in a module are part
of the module's compile time, while function definitions and top-level
expressions are part of the module's run time. The following variant
of our example illustrates this difference, where blue regions
highlight run-time code, and purple highlights compile-time code:
@footnote{In black and white, ``blue'' is darker than the background,
and ``purple'' is even darker.}
@;
@centerline{@(phase-slide)}
@;
In @filepath{kitchen.rkt}, the blue regions include references to
@racket[groceries] and @racket[fold], and therefore they are imported
with a plain @racket[require]. Indeed, the author of
@filepath{kitchen.rkt} does not have to know whether @racket[fold] is
macro---only that it's referenced from a run-time position. In
contrast, @filepath{grocery.rkt} uses @racket[fold] in a compile-time
position, so it must use @racket[require]...@racket[for-syntax] to
import @racket[fold] from @filepath{list.rkt}. If
@filepath{grocery.rkt} used @racket[require] without
@racket[for-syntax] to import @filepath{list.rkt}, @racket[fold] in
@filepath{grocery.rkt} would be unbound, triggering an error.

The difference between @racket[require]...@racket[for-syntax] and
@racket[eval-when]...@racket[use] is subtle but crucial: with
@racket[eval-when], a programmer attempts to say when code should run
to make identifiers available; with @racket[require], a programmer
says in which phase an identifier should be bound, leaving the
questions of loading and running up to the language. That is, with
@racket[require], programmers reason about scope, instead of reasoning
about side-effecting loads.

If the @racket[groceries] macro in @filepath{grocery.rkt} expands to a
run-time use of @racket[fold] instead of using @racket[fold] at
compile time, then @filepath{grocery.rkt} should import
@filepath{list.rkt} normally, instead of @racket[for-syntax]. Put
another way, a syntax-quoting @tt{#'} in a macro embeds a run-time
region within compile-time code, and a @tt{#'}-quoted reference to
@racket[fold] in a macro is the same as a direct reference in a
run-time position:
@;
@centerline{@(phase-slide #:kitchen? #f #:template? #t)}
@;
Some libraries, such as a list-processing library like
@filepath{list.rkt}, may even be useful at both compile time and run
time. In that case, the module can be @racket[require]d both normally
and @racket[for-syntax]:
@;
@centerline{@(phase-slide #:kitchen? #f #:split? #t)}
@;

When a module is used in multiple phases, then it is instantiated
separately in each phase. Furthermore, to ensure that all-at-once
compilation is consistent with separate compilation of modules, a
module that is used at compile time is instantiated separately for
each module to be compiled. Separate instantiations avoid cross-phase
interference and help tame state enough to make it useful@~cite[macromod
typed-scheme-adv].

Using lexical scope to determine and manage evaluation phases fits
naturally with hygienic macros, which also obey lexical scope. For
example, if the @racket[groceries] macro expands to a use of a private
function that is defined within @filepath{grocery.rkt}, then hygiene
ensures that the expanded reference is bound by the definition in
@filepath{grocery.rkt} and not a definition in the client module where
the @racket[groceries] macro is used. Furthermore, the fact that
@racket[groceries] was bound for use in a certain position implies
@filepath{grocery.rkt} was imported for that position's phase, which
in turn implies that the private definition from
@filepath{grocery.rkt} will be ready by the time that the position is
evaluated.

Preserving lexical scope across module boundaries ultimately leads to
the need for an import form that is like
@racket[require]...@racket[for-syntax] but that works in the other
direction. For example, imagine that the core implementation of the
@racket[groceries] macro is both sophisticated and general enough that
it should be put in its own module, @filepath{gen-list-code.rkt} that
is @racket[require]d...@racket[for-syntax] for use by the
@racket[groceries] macro:
@;
@centerline{@(template-slide)}
@;
As @racket[gen-code] constructs an expression for the expansion of
@racket[groceries], it uses a syntax-quoted @racket[fold]. The
@racket[gen-code] implementation is at the run-time phase relative to
its enclosing module, but the module as a whole will be instantiated
at compile time for @racket[groceries]. The compile-time shift of
@filepath{gen-list-code.rkt} for @filepath{grocery.rkt} will bring the
syntax-quoted @racket[fold] into a compile-time context, in which case
the syntax quote correctly produces a run-time reference to
@racket[fold]. Relative to the @filepath{gen-list-code.rkt} module,
however, the syntax-quoted @racket[fold] is at a phase @emph{later}
than run time for the module; we call that time @defterm{template
time}, since it corresponds to the phase for identifiers that are
syntax-quoted at run time. Accordingly, @filepath{gen-list-code.rkt}
must @racket[require] the @filepath{list.rkt} module with
@racket[for-template].

Following the dependencies from @filepath{grocery.rkt} through
@filepath{list.rkt}, the @racket[for-syntax] and @racket[for-template]
phase shifts effectively cancel each other. A @racket[for-syntax]
import implies a phase shift of +1, and a @racket[for-template] import
implies a phase shift of -1,@footnote{Our phase numbers are negated
compared to phase numbers in the staged-computation literature; sorry
about that. The negation reflects our emphasis on macros instead of
run-time code generation.}  and +1 + -1 = 0. Consequently, a phase 0
(i.e., run time) use of @filepath{grocery.rkt} triggers a phase 0 use
of @filepath{list.rkt}, which means that a use of @racket[groceries]
and its expansion to @racket[fold] are consistent.

Using a macro inside of a compile-time context, such as the right-hand
side of a @racket[define-syntax] form, means that the macro runs at
compile time relative to compile time---that is, at phase 2. In
@racket[require], @racket[for-syntax] and @racket[for-template]
specifications can be nested to import bindings into arbitrary phase
levels. Furthermore, @racket[begin-for-syntax] allows compile-time
definitions to be written within a module whose macros need the
definitions. For example, if @racket[gen-code] is needed only by
macros within @filepath{grocery.rkt}, it might be better implemented
within the @filepath{grocery.rkt} module instead of in a separate module:
@;
@centerline{@(begin-for-syntax-slide)}
@;
This variant of @filepath{grocery.rkt} effectively inlines the earlier
@filepath{gen-list-code.rkt} module, shifting the inlined code with
@racket[begin-for-syntax] instead of
@racket[require]...@racket[for-syntax].

In the same way that a module can import bindings at multiple phases,
it can also @emph{export} bindings at multiple phases. The most prominent
example of multi-phase exports is the @racket[racket] module, which is
implicitly imported in the above module pictures. The @racket[racket]
module re-exports all of the bindings of a more primitive
@racket[racket/base] module, and it re-exports the bindings at both
phase 0 and phase 1. That's why a module that imports @racket[racket]
can include a definition of the form
@;
@scodeblock{(begin-for-syntax (define gen-code ...))}
@;
The @racket[racket] module exports @racket[define] at phase 1,
so the @racket[define] above is bound for the phase where it is used.
If the module's initial import is @racket[racket/base], instead of @racket[racket],
then the macro definition above is a syntax error unless @racket[define]
is explicitly imported @racket[for-syntax].

Naturally, @racket[begin-for-syntax] forms can be nested to implement
a function that is needed for a macro that is used on the right-hand
side of a macro implementation, and so on, provided that
@racket[begin-for-syntax] itself is imported into each phase where it
is used. Despite many possibilities for phases and nesting, the
lexical and phase-sensitive constraints on each variable
ensure that evaluation times are properly kept in sync.
