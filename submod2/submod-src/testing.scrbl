#lang scribble/sigplan
@(require "utils.rkt"
          "test-pict.rkt")

@(define (xfootnote . a) "")

@title[#:tag "testing"]{Testing Time And Documentation Time}

We can deploy the principles of lexical scope and phase separation to
distinguish phases other than just run time and compile time. For
example, suppose that we implement a function that uses the Racket
@racket[current-seconds] function to implement a
@racket[current-hours] function, and suppose that the implementation
uses a private @racket[seconds->hours] function:
@;
@centerline{@(hours-slide)}
@;
The internal @racket[seconds->hours] function should be tested:
@;
@scodeblock{
   (require rackunit)
   (check-equal 0 (seconds->hours 0))
   (check-equal 1 (seconds->hours 3600))
   (check-equal 42 (seconds->hours 151200))
}
@;
Where should we put these tests? If we put them in a separate module,
then @racket[seconds->hours] must be exported, but we wanted to keep
that function private. If we add the tests to the end of
@filepath{hours.rkt}, then the private function
@racket[seconds->hours] is available, but the tests will run every
time that the @filepath{hours.rkt} module is used, and the dependency
on @racket[rackunit] means that every program that uses
@filepath{hours.rkt} must carry along the testing framework.

The Racket @racket[module+] form offers a solution that lets a
programmer include tests with the code, which makes the most sense in
terms of scoping, but puts the code in a separate @defterm{submodule}
that effectively implements a testing phase (highlighted orange):
@;
@centerline{@(test-submod-slide)}
@;
When @filepath{hours.rkt} is imported into another module with
@racket[require], then the @racket[test] submodule is ignored; the
code for the @racket[test] module is not even loaded in that case,
assuming that the module has been compiled ahead of time. If the
@racket{hours.rkt} module is run with the @exec{raco test}
command-line tool or run in DrRacket, then the @racket[test] module is
automatically run.@footnote{Sometimes you do want to run tests
when a particular library is used. For example, this paper
depends on the implementation of the model that is presented
in @secref["model"], and the paper explicitly imports the implementation's 
@racket[test] submodule so that the model is tested whenever the paper
is rendered@~cite[run-your-research].}

The testing example uses a submodule relatively explicitly.@xfootnote{The
@racket[module+] form is a macro, rather than a core submodule
declaration, but it's close to the core submodule form.} As an example
of a new phase that looks
more like compile time, consider the problem of documenting library
exports at the site of their implementations. Common Lisp supports
documentation in the form of @defterm{docstrings}, which are string
literals that start the body of a function:
@;
@scodeblock{
     (define (current-seconds)
       "reports the time in seconds since the Epoch"
        ....)
     (define (current-days)
       "reports the time in days since the Epoch"
       ....)
}
@;
Docstrings are often accessed at run time, but they are required to be
literal strings so that they can be recognized syntactically. With
the restriction to literal docstrings, a tool can build a 
documentation index without
running a program. Constraining docstrings to literal strings,
however, prevents abstraction:
@;
@scodeblock{
     (define (docs-for-current what)
       (format "reports the time in ~a since the Epoch" 
               what))

     (define (current-seconds)
       (docs-for-current "seconds")
        ....)

     (define (current-days)
       (docs-for-current "days")
       ....)
}
@;
The @racket[docs-for-current] abstraction doesn't work if
docstrings are restricted to literal strings.
In Racket, we can address the problem by developing a
macro-based extension of the language: a
@racket[begin-for-doc] form for documentation-time code
along with a keyword syntax (in a macro replacement of
@racket[define]) to identify a part of a function definition as
documentation:
@;
@scodeblock{
     (begin-for-doc
       (define (docs-for-current what)
         (format "reports the time in ~a since the Epoch"
                 what)))

     (define (current-seconds)
       #:doc (docs-for-current "seconds")
        ....)
}
@;
With this approach, documentation need not be constrained to plain text.
@Figure-ref["srcdoc"] provides a more complete example that uses Scribble@~cite[scribble]
syntax for writing documentation, and where the @racket[begin-for-doc], @racket[for-doc],
and @racket[define] forms are implemented in @filepath{doc-define.rkt}.
The @racket[require]...@racket[for-doc] import binds Scribble typesetting
forms such as @racket[code] for use in writing documentation, where
@racket[code] in turn uses the same scope information as used at
run-time to hyperlink the documentation's mention of
@racket[current-seconds] to the documentation of
@racket[current-seconds]. Similarly, the contract expressions after
@racket[#:contract] are mainly run-time code, but they are also
incorporated into the documentation, where @racket[->] is hyperlinked
to documentation on the contract-construction form and
@racket[exact-integer?] is hyperlinked to documentation of the
number-testing predicate.

@figure[
"srcdoc"
@list{Example program with documentation time}
@nested[#:style "smaller"]{
@scodeblock|{
#lang at-exp racket/base
(require "doc-define.rkt"
         racket/contract/base
         (for-doc racket/base
                  scribble/manual))

(define (seconds->hours secs)
  #:contract (-> exact-integer? exact-integer?)
  #:doc @{Takes @code{secs}, a number of seconds
          since the Epoch, and converts it to a
          number of days since the Epoch.
                
          For example, compose with with the
          @code{current-seconds} function to get
          @code{current-hours}.}
  (quotient secs (* 60 60)))

(begin-for-doc
 (define (current-docs what)
   @list{Returns @what since the epoch.}))

(define (current-seconds)
  #:contract (-> exact-integer?)
  #:doc @{@current-docs["seconds"]}
  (inexact->exact
   (floor (/ (current-inexact-milliseconds) 1000))))

(define (current-hours)
  #:contract (-> exact-integer?)
  #:doc @{@current-docs["hours"]}
  (seconds->hours (current-seconds)))
}|}]

The example in @figure-ref["srcdoc"] does not use submodules
explicitly, but the @racket[form-doc] imports, @racket[begin-for-doc]
forms, and documentation-time  @racket[#:doc] expressions are all
macro-expanded into a @racket[srcdoc] submodule (as we sketch later
in @secref["api"]). The submodule is then
available for use by documentation tools. The module that provides
@racket[current-hours], etc., does not itself depend on the Scribble
libraries or use the code for @racket[current-docs] when the module is
loaded.
