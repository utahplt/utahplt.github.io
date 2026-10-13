#lang at-exp racket/base
(require slideshow/pict
         redex/reduction-semantics
         redex/pict
         "model.rkt"
         "rewrites.rkt")

(provide (except-out (all-defined-out)
                     m:ast
                     t)
         (rename-out [m:ast ast]))


(define-language desc-L
  [mnam desc-mname]
  [dnam desc-dname]
  [ast desc-ast]
  [M desc-M]
  [Σ desc-Σ])
(define (desc-nam-pict)
  (language->pict desc-L #:nts '(mnam dnam)))
(define (desc-ast-pict)
  (language->pict desc-L #:nts '(ast)))
(define (desc-M-pict)
  (language->pict desc-L #:nts '(M Σ)))

(define-language L-module
  [form (begin-for-syntax form ...)
        (define dnam expr)
        (define-syntax dnam expr)
        (require req)
        (module mnam mnam form ...)
        (module* mnam mnam form ...)
        (module* mnam () form ...)
        (dnam arb ...)]
  [req mnam
       (for-syntax req)
       (for-template req)]
  [expr desc-expr]
  [arb arb-expr])

(define (t s)
  (with-rewrites
   (text s 'roman (default-font-size))))

(define (nt s)
  (with-rewrites
   (text s (non-terminal-style) (default-font-size))))

(define (mf s)
  (with-rewrites
   (text s (metafunction-style) (metafunction-font-size))))

(define (m:ast s)
  (with-rewrites (ast s)))

(define-syntax-rule (tm e)
  (with-rewrites
   (term->pict L e)))

(define (mf/sym s)
  (mf (symbol->string s)))

(define-syntax signature
  (syntax-rules (: ->)
    [(_ nm : arg ... -> res)
     (hbl-append 4 (mf/sym 'nm) (t ":") (tm arg) ... (t "→") (tm res))]))

(define (s-exp-grammar-pict)
  (with-rewrites
   (vl-append
    10
    (language->pict L #:nts '(s-exp nam))
    (language->pict L #:nts '(mnam dnam))
    (language->pict L #:nts '(stx id))
    (language->pict L #:nts '(binding bindings))
    (language->pict L #:nts '(body)))))

(define (module-grammar-pict)
  (with-rewrites
   (vl-append
    10
    (language->pict L #:nts '(mod))
    (ht-append
     15
     (vl-append
      10
      (language->pict L #:nts '(dep defn))
      (language->pict L #:nts '(kind ph)))
     (vl-append
      10
      (desc-nam-pict)
      (desc-ast-pict)))
    (desc-M-pict))))

(define (sig-line w)
  (linewidth 0.5 (hline w 0)))

(define (purpose->pict e)
  (apply vl-append
         2
         (for/list ([e (in-list e)])
           (apply hbl-append
                  (for/list ([p (in-list e)])
                    (if (pict? p)
                        p
                        (text (regexp-replace* #rx"'" p "\u2019")
                              'roman 
                              (default-font-size))))))))

(define (append-signature s m)
  (define l (sig-line (max (pict-width (unscale s))
                           (pict-width m))))
  (vl-append 2
             (unscale s)
             l
             m))

(define (append-signature+purpose s e m)
  (define p (purpose->pict e))
  (define l (sig-line (max (pict-width (unscale s))
                           (pict-width p)
                           (pict-width m))))
  (vl-append 2
             (unscale s)
             l
             p
             l
             m))

(define (append-signature+purpose/only s e m)
  (define p (purpose->pict e))
  (define l (sig-line (max (pict-width (unscale s))
                           (pict-width p))))
  (vl-append 2
             (unscale s)
             l
             p))

(define (cmodule-pict)
  (with-rewrites
   (append-signature
    (signature cmodule : s-exp M -> M)
    (parameterize ([linebreaks '(#t)])
      (metafunction->pict cmodule)))))

(define (run-pict)
  (with-rewrites
   (parameterize ()
     (vl-append
      10
      (append-signature+purpose
       (signature run : mnam ph M Σ -> Σ)
       @(list
         @list{Run @tm[mnam] (as declared in @tm[M]) at phase @tm[ph_Δ] and record in @tm[Σ],}
         @list{but only if @tm[[At mnam ph_Δ]] is not already recorded as ran in @tm[Σ].})
       (metafunction->pict run))
      (append-signature+purpose
       (signature run* : (Seq dep ...) ph M Σ -> Σ)
       @(list
         @list{Run each @tm[dep] (which is a @tm[[At mnam ph]]) with phase shift @tm[ph].})
       (metafunction->pict run*))
      (append-signature+purpose
       (signature run-body : (Seq defn ...) mnam ph Σ -> Σ)
       @(list
         @list{Evaluate each run-time @tm[defn] with phase shift @tm[ph_Δ], as long as}
         @list{@tm[ph_Δ] plus the enclosing module's instantiation phase lands at}
         @list{phase 0. Record definitions in @tm[Σ].})
       (parameterize ([linebreaks '(#f #t #t)])
         (metafunction->pict run-body)))))))

(define (bindings-pict)
  (with-rewrites
   (parameterize ()
     (vl-append
      10
      (append-signature+purpose/only
       (signature import : mnam ph M -> bindings)
       @(list
         @list{Collect a module's exports into a set of @tm[bindings].})
       (metafunction->pict import))
      (append-signature+purpose/only
       (signature wrap : s-exp bindings -> stx)
       @(list
         @list{Attach bindings to a raw S-expression.})
       (metafunction->pict wrap))
      (append-signature+purpose/only
       (signature strip : stx -> s-exp)
       @(list
         @list{Strip bindings from a syntax object to get an S-expression.})
       (metafunction->pict strip))
      (append-signature+purpose/only
       (signature resolve : id ph -> binding)
       @(list
         @list{Find relevant binding at a given phase, @tm[ph].})
       (parameterize ([linebreaks '(#t)]
                      [metafunction-cases '(0)])
         (metafunction->pict resolve)))))))

(define (s-exp-pict)
 (vc-append 10
            (s-exp-grammar-pict)
            (bindings-pict)))

(define (module-language-pict)
  (with-rewrites
   (language->pict L-module)))

(define (cbody-signature-pict)
  (vc-append
   10
   (with-rewrites
    (language->pict L #:nts '(deps defns)))
   (signature cbody : (Seq body ...) mnam deps defns M Σ -> M)))

(define (cbody-done-pict)
  (with-rewrites
   (vc-append
    10
    (parameterize ([linebreaks '(#t)]
                   [metafunction-cases '(0)])
      (metafunction->pict cbody)))))

(define (cbody-begin-syntax-pict)
  (with-rewrites
   (parameterize ([linebreaks '(#t)]
                  [metafunction-cases '(2)])
     (metafunction->pict cbody))))

(define (cbody-define-pict)
  (with-rewrites
   (parameterize ([linebreaks '(#t)]
                  [metafunction-cases '(3)])
     (metafunction->pict cbody))))

(define (cbody-define-syntax-pict)
  (with-rewrites
   (parameterize ([linebreaks '(#t)]
                  [metafunction-cases '(4)])
     (metafunction->pict cbody))))

(define (cbody-macro-pict)
  (with-rewrites
   (parameterize ([linebreaks '(#t)]
                  [metafunction-cases '(5)])
     (metafunction->pict cbody))))

(define (cbody-require-pict)
  (with-rewrites
   (vc-append
    10
    (parameterize ([linebreaks '(#t)]
                   [metafunction-cases '(6)])
      (metafunction->pict cbody)))))

(define (parse-req-pict)
  (with-rewrites
   (append-signature
    (signature parse-req : stx ph -> [Tup nam ph])
    (metafunction->pict parse-req))))

(define (cbody-module-pict)
  (with-rewrites
   (let ()
     (define (one n)
       (parameterize ([linebreaks '(#t)]
                      [metafunction-cases (list n)])
         (metafunction->pict cbody)))
     (vc-append 15 (one 7) (one 8) (one 9)))))

