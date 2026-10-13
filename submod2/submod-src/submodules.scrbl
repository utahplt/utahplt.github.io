#lang scribble/sigplan @preprint
@(require "utils.rkt"
          (only-in (submod "model.rkt" test)))

@title{Submodules in Racket @subtitle{You Want it @emph{When}, Again?}}
@authorinfo["Matthew Flatt" "PLT and University of Utah" "mflatt@cs.utah.edu"]

@abstract{

 In an extensible programming language, programmers write code that
 must run at different times---in particular, at compile time versus
 run time. Modules in the Racket programming language enable
 programmers to reason about programs in the face of such
 extensibility, because the distinction between run-time and
 compile-time phases is built into the language
 model. @defterm{Submodules} extend
 Racket's module system to make the phase-separation facet of the
 language extensible. That is, submodules give programmers the
 capability to define new phases, such as ``test time'' or
 ``documentation time,'' with the same reasoning and code-management
 benefits as the built-in distinction between run time and compile
 time.

}

@include-section["intro.scrbl"]
@include-section["macromod.scrbl"]
@include-section["testing.scrbl"]
@include-section["api.scrbl"]
@include-section["model.scrbl"]
@include-section["discuss.scrbl"]
@include-section["related.scrbl"]
@include-section["end.scrbl"]

@generate-bibliography[]

@generate-footnotes[]
