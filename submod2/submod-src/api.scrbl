#lang scribble/sigplan
@(require "utils.rkt"
          "test-pict.rkt")

@title[#:tag "api"]{Submodules}

Our pictorial representation of modules so far omits the @hash-lang[]
line that starts an actual Racket module. For example, the module
@;
@centerline{@(clock-slide)}
@;
represents the file @filepath{clock.rkt} whose content is
@;
@scodeblock{
  #lang racket
  "tick"
}
@;
The initial @racket[@#,hash-lang[] racket] line, in turn, is a
shorthand for using the parenthesized @racket[module] form:
@;
@scodeblock{
  (module clock racket
    "tick")
}
@;
The @racket[racket] here indicates a module to supply the initial
bindings for the @racket[clock] module body. The @racket[clock]
module does not use any of those bindings (except for the implicit
literal-expression form), but it could use any @racket[racket]
function or syntactic form---and only those syntactic forms, until it
uses @racket[require] to import more. Running the @racket[clock]
module prints the result of the expression in its body, so it prints
@racket["tick"].

To a first approximation, a @defterm{submodule} is simply a nested
@racket[module] form.
@;
@scodeblock{
  (module clock racket
    "tick"
    (module tock racket
      "tock"))
}
@;
Running the @racket[clock] module still prints just
@racket["tick"].  Similarly, evaluating @racket[(require "clock.rkt")]
in the Racket REPL prints @racket["tick"]. In those cases, the
@racket[tock] submodule is declared, but it is not instantiated and
run. Evaluating @racket[(require (submod "clock.rkt" tock))] in the
Racket REPL prints @racket["tock"] and does not print @racket["tick"].
Submodule nesting implies a kind of nesting of module names, but it
does not imply any run-time relationship between a submodule and its
enclosing module.

A module can explicitly run one of its submodules using
@racket[require], the same as it would trigger any other module. A module
can reference one of its submodules using the relative form
@racket[(submod "."  ....)]:
@;
@scodeblock{
  (module clock racket
    (module tock racket
      "tock")
    (require (submod "." tock))
    "tick")
}
@;
Running this module prints both @racket["tock"] and @racket["tick"],
since the module explicitly @racket[require]s its submodule and
therefore creates an instantiation relationship.@;
@;
@footnote{The @racket[require] form is a dependency declaration, not a
side-affecting statement, and its placement relative to the
@racket["tick"] expression does not matter. Putting @racket["tick"]
before the @racket[tock] submodule declaration will still print
@racket["tock"] first, since a @racket[require]d module is
instantiated before the importing module. Of course, putting side
effects such as printing in a module body is usually discouraged---and
order matters even less when a module has no side-effecting top-level
form---but we exploit a printing side effect here for simplicity.}

A submodule declared with @racket[module] cannot import from its
enclosing module. The @racket[module*] form is the same as
@racket[module] for declaring submodules, but it allows the submodule
to import from its enclosing module and not vice versa. (Module
dependencies in Racket must be acyclic.) Thus, with the @racket[clock]
variant
@;
@scodeblock{
  (module clock racket
    "tick"
    (module* tock racket
      (require (submod ".."))
      "tock"))
}
@;
then @racket[(require (submod "clock.rkt" tock))] prints
@racket["tick"] followed by @racket["tock"].

For the same reason that module nesting does not imply a connection in
instantiation times, module nesting alone does not imply a lexical-binding
connection. For example,
@;
@scodeblock{
  (module clock racket
    (define sound "tick")
    (module* tick racket
      sound))
}
@;
is a syntax error, because the @racket[tock] module starts with
only the bindings of @racket[racket], just like any module that
declares @racket[racket] as its language. Furthermore,
@;
@scodeblock{
  (module clock racket
    (define sound "tick")
    (module* tick racket
      (require (submod ".."))
      sound))
}
@;
is also a syntax error, because @racket[clock] does not export
its @racket[sound] binding. A submodule can specify @racket[#f]
as its initial language to indicate that the enclosing module body provides
the submodule's bindings:
@;
@scodeblock{
  (module clock racket
    (define sound "tick")
    (module* tick #f
      sound))
}
@;
In this case, @racket[(racket (submod "clock.rkt" tock))] prints
@racket["tick"], which is the value of @racket[sound].

The @racket[module+] form, which we used in @secref["testing"], is a
macro that expands to @racket[module*] with @racket[#f] as its
language.  The @racket[module+] form also collects multiple declarations
with the same module name and concatenates the bodies in a single submodule
declaration, so @racket[(module+ test ....)] can be used multiple times
in a module to build up one @racket[test] submodule.

Submodules can be nested under @racket[begin-for-syntax]. For a
submodule declared with @racket[module] or with @racket[module*] and a
non-@racket[#f] initial import, the nesting has no effect on the
submodule, since the submodule starts with a fresh lexical
context. Nesting @racket[module*] with a @racket[#f] initial import
under @racket[begin-for-syntax] has the effect of shifting the
enclosing module's bindings @emph{down} by one phase in the body of
the submodule. That is, a submodule's body by definition starts at
phase 0 relative to the submodule, so if the submodule is lexically at phase
@italic{ph} relative to an enclosing module, then the enclosing module
is at phase -@italic{ph} relative to the submodule.


@figure[
"srcdoc-expanded"
@list{Sketch of macro expansion for @figure-ref["srcdoc"]}
@nested[#:style "smaller"]{
@scodeblock|{
#lang at-exp racket/base
(require "doc-define.rkt"
         racket/contract/base)

(provide
 (contract/out
  [seconds->hours (-> exact-integer? exact-integer?)]
  [current-seconds (-> exact-integer?)]
  [current-hours (-> exact-integer?)]))

(define (seconds->hours secs) (quotient secs (* 60 60)))
(define current-seconds ....)
(define current-hours ....)

(begin-for-syntax
 (module srcdoc #f
  (require scribble/base
           scribble/manual)

  (define (current-docs what)
    @list{Returns @what since the epoch.})

  (define doc
   (list
    (format-function-doc
     @code{seconds->hour}
     (list @code{src})
     @code{(-> exact-integer? exact-integer?)}
     @{Takes @code{secs}, a number of seconds
       since the Epoch, ....})
    (format-function-doc
     @code{current-seconds}
     @code{()}
     @code{(-> exact-integer?)}
     @{@current-docs["seconds"]})
    (format-function-doc
     @code{current-hours}
     @code{()}
     @code{(-> exact-integer?)}
     @{@current-docs["hours"]})))))
}|}]

Relative phase shifts are useful in the case of documentation
submodules, where all of the bindings inside of a module are relevant
for the submodule, but the submodule should not have a direct
execution dependence on the enclosing module. For example, building
documentation should not require running the documented library.
@Figure-ref["srcdoc-expanded"] sketches the expansion of the module
with documentation in @figure-ref["srcdoc"]. In the expansion,
contracts for each function declaration are moved to a
@racket[provide]...@racket[contract-out] form, the function
definitions contain only the function implementations, and all
documentation is moved into a @racket[srcdoc] submodule.
The submodule is declared under @racket[begin-for-syntax], so that the
enclosing module is at template time---i.e., an unspecified time in
the future---relative to the implementation of its documentation;
the @racket[code] form can then reflect on binding information inherited
from the enclosing module to properly hyperlink references to identifiers
that are used in the function contracts and documentation prose.
