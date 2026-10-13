#lang at-exp racket
(require scribble/manual
         scriblib/autobib
         scriblib/footnote
         scriblib/figure
         "bib.rkt")

(provide (all-from-out scribble/manual
                       scriblib/autobib
                       scriblib/figure
                       "bib.rkt")
         ~cite citet generate-bibliography
         footnote generate-footnotes
         scodeblock
         R6RS)

(define-cite ~cite citet generate-bibliography)
(define-footnote footnote generate-footnotes)

(define-syntax-rule (scodeblock c ...)
  (nested #:style "smaller" (codeblock c ...)))

(define R6RS @elem{R@superscript{6}RS})
