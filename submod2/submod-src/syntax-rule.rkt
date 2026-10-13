#lang racket

(provide define-syntax-rule/keep-srcloc)

;; Like `define-syntax-rule', but an identifier substituted
;; in place of a pattern variable takes on the source location
;; of the pattern variable. This kind of macro can abstract
;; over metafunctions and allow typesetting to still work.

(define-syntax (define-syntax-rule/keep-srcloc stx)
  (syntax-case stx ()
    [(_ (id arg ...) tmpl)
     (with-syntax ([(actual-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(define-syntax (id stx)
           (syntax-case stx ()
             [(_ actual-arg ...)
              (substitute (quote-syntax tmpl) (list #'arg ...) (list #'actual-arg ...))])))]))

(define-for-syntax (substitute tmpl args actuals)
  (let loop ([tmpl tmpl])
    (cond
     [(identifier? tmpl)
      (or (for/or ([arg (in-list args)]
                   [actual (in-list actuals)])
            (and (free-identifier=? tmpl arg)
                 (datum->syntax actual
                                (syntax-e actual)
                                tmpl
                                actual)))
          tmpl)]
     [(syntax? tmpl)
      (datum->syntax tmpl
                     (loop (syntax-e tmpl))
                     tmpl
                     tmpl)]
     [(pair? tmpl)
      (cons (loop (car tmpl)) (loop (cdr tmpl)))]
     [else tmpl])))
