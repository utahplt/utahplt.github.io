#lang at-exp racket
(require scribble/manual
         scriblib/autobib)

(provide (all-defined-out))

;; ----------------------------------------

(define IEEE "IEEE ")
(define ACM "ACM ")
(define International "Intl. ")
(define Conference "Conf. ")
(define Workshop "Wksp. ")
(define Journal "J. ")
(define Symposium "Sym. ")
(define Transactions "Trans. ")

(define icfp (string-append ACM International Conference "Functional Programming"))
(define pldi (string-append ACM Conference "Programming Language Design and Implementation"))
(define popl (string-append ACM Symposium "Principles of Programming Languages"))
(define lncs "Lecture Notes in Computer Science")
(define sigplan-notices (string-append ACM "SIGPLAN Notices"))
(define scheme-workshop (string-append Workshop "Scheme and Functional Programming"))
(define jfp (string-append Journal "Functional Programming"))
(define ifl (string-append International Symposium "Functional and Logic Programming"))
(define tfp (string-append Symposium "Trends in Functional Programming"))
(define ecoop (string-append "European " Conference "Object-Oriented Programming"))
(define oopsla (string-append ACM Conference "Object-Oriented Programming, Systems, Languages and Applications"))
(define ieee-software (string-append IEEE "Software"))
(define toplas (string-append Transactions "Programming Languages and Systems"))
(define dls "Dynamic Languages Symposium")
(define gpce "Generative Programming and Component Engineering")
(define flops (string-append Symposium "Functional and Logic Programming"))
(define esop (string-append "European " Symposium "on Programming"))
(define iclp (string-append  International Conference "on Logic Programming"))
(define fse (string-append International Symposium "on the Foundations of Software Engineering"))
(define foal "Foundations of Aspect-Oriented Languages")
(define aosd "Aspect-Oriented Software Development")
(define sp&e "Software—Practice and Experience")
(define tcs "Theoretical Computer Science")
(define lfp "Lisp and Functional Programming")
(define haskell-workshop (string-append ACM Workshop "Haskell"))

;; ----------------------------------------

(define macromod
  (make-bib
   #:author "Matthew Flatt"
   #:title @elem{Composable and Compilable Macros: You Want it @emph{When?}}
   #:location (proceedings-location icfp
                                    #:pages '(72 83))
   #:date "2002"))

(define scribble
  (make-bib
   #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
   #:title @elem{Scribble: Closing the Book on Ad Hoc Documentation Tools}
   #:location (proceedings-location icfp
                                    #:pages '(109 120))
   #:date "2009"))

(define typed-scheme-adv
  (make-bib
   #:author (authors "Ryan Culpepper" "Sam Tobin-Hochstadt" "Matthew Flatt")
   #:title "Advanced Macrology and the Implementation of Typed Scheme"
   #:location (proceedings-location scheme-workshop)
   #:date "2007"))

(define typed-racket
  (make-bib 
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:title "The Design and Implementation of Typed Scheme"
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date "2008"))

(define redex-book
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

(define syntax-case
  (make-bib
   #:author (authors "R. Kent Dybvig" "Robert Hieb" "Carl Bruggeman")
   #:title "Syntactic Abstraction in Scheme"
   #:location (journal-location
               "Lisp and Symbolic Computation"
               #:volume "5"
               #:number "4"
               #:pages '(295 326))
   #:date "1993"))

(define expmodel
  (make-bib
   #:author (authors "Matthew Flatt" "Ryan Culpepper" "Robert Bruce Findler" "David Darais")
   #:title "Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts"
   #:location (journal-location
               jfp
               #:volume "22"
               #:number "2"
               #:pages '(181 216))
   #:date "2012"))

(define run-your-research
  (make-bib
   #:author (authors "Casey Klein" "John Clements" "Christos Dimoulas"
                     "Carl Eastlund" "Matthias Felleisen" "Matthew Flatt"
                     "Jay McCarthy" "Jon Rafkind" "Sam Tobin-Hochstadt"
                     "Robert Bruce Findler")
   #:title "Run Your Research: On the Effectiveness of Lightweight Mechanization"
   #:location (proceedings-location
               popl)
   #:date "2012"))

(define R6RS-standard
  (make-bib
   #:author (editor "Michael Sperber")
   #:title (elem "The Revised " (superscript "6") " Report on the Algorithmic Language Scheme")
   #:date "2007"))

(define implicit-phasing
  (make-bib
   #:author (authors "Abdulaziz Ghuloum" "; R. Kent Dybvig")
   #:title "Implicit Phasing for R6RS Libraries"
   #:location (proceedings-location
               icfp
               #:pages '(303 314))
   #:date "2007"))

(define MetaML
  (make-bib
   #:author (authors "Walid Taha" "Tim Sheard")
   #:title "MetaML and Multi-Stage Programming with Explicit Annotations"
   #:location (journal-location
               tcs
               #:volume "248"
               #:number "1-2"
               #:pages '(211 242))
   #:date 2000))

(define LMS
  (make-bib
   #:title "Lightweight Modular Staging: a Pragmatic Approach to Runtime Code Generation and Compiled DSLs"
   #:author (authors "Tiark Rompf" "Martin Odersky")
   #:location (proceedings-location
               gpce
               #:pages '(127 136))
   #:date "2010"))

(define Rompf-dissertation
  (make-bib
   #:author "Tiark Rompf"
   #:title (~a "Lightweight Modular Staging and Embedded Compilers:"
               " Abstraction without Regret for High-Level High-Performance Programming")
   #:location (dissertation-location
               #:institution "École Polytechnique Fédérale de Lausanne")
   #:date "2012"))

(define SugarJ
  (make-bib
   #:title "SugarJ: Library-Based Syntactic Language Extensibility"
   #:author (authors "Sebastian Erdweg" "Tillmann Rendel" "Christian Kästner" "Klaus Ostermann")
   #:location (proceedings-location 
               oopsla
               #:pages '(391 406))
  #:date "2011"))

(define TemplateHaskell
  (make-bib
   #:title "Template metaprogramming for Haskell"
   #:author (authors "Simon Peyton Jones" "Tim Sheard")
   #:location (proceedings-location 
               haskell-workshop
               #:pages '(1 16))
   #:date "2002"))

(define Converge
  (make-bib
   #:author "Laurence Tratt"
   #:title "Compile-time Meta-programming in a Dynamically Typed OO Language"
   #:location (proceedings-location
               dls
               #:pages '(49 63))
   #:date "2005"))

(define Converge-DSL
  (make-bib
    #:author (authors "Laurence Tratt")
    #:title "Domain Specific Language Implementation via Compile-time Meta-programming"
    #:location (journal-location
                toplas
                #:volume "30"
                #:number "6"
                #:pages '(1 40))
    #:date "2008"))

(define Chez-modules
  (make-bib
   #:title "Extending the Scope of Syntactic Abstraction"
   #:author (authors "Oscar Waddell" "R. Kent Dybvig")
   #:location (proceedings-location
               popl
               #:pages '(203 213))
   #:date "1999"))

(define hygiene
  (make-bib
   #:author (authors "Eugene Kohlbecker" "Daniel P. Friedman" "Matthias Felleisen" "Bruce Duba")
   #:title "Hygienic Macro Expansion"
   #:location (proceedings-location lfp #:pages '(151 181))
   #:date "1986"))

(define Haddock
  (make-bib
   #:author "Simon Marlow"
   #:title "Haddock, a Haskell Documentation Tool"
   #:location (proceedings-location
               haskell-workshop
               #:pages '(78 89))
   #:date "2002"))

(define Maya
  (make-bib
   #:title "Maya: Multiple-Dispatch Syntax Extension in Java"
   #:author (authors "Jason Baker" "Wilson C. Hsieh")
   #:location (proceedings-location
               pldi
               #:pages '(270 281))
   #:date "2002"))
