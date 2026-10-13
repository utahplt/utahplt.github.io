#lang scribble/base
@(require "model.rkt"
          "rewrites.rkt"
          (except-in "model-pict.rkt" ast)
          redex/pict
          slideshow/pict)

@(define (cinset p) (scale (inset p 0 5) 1.2))
@(define (sig p)
   (inset (cinset (vl-append (hline 300 0) p)) 0 5 0 0))
@(define-syntax-rule (mf-cases lbs mf)
   (begin
     ;; check that we have the right number of linebreaks:
     (void
      (parameterize ([linebreaks 'lbs])
        (metafunction->pict mf)))
     ;; Render each line separately:
     (for/list ([lb 'lbs]
                [i (in-naturals)])
       (list (with-rewrites
              (parameterize ([linebreaks (list lb)]
                             [metafunction-cases (list i)])
                (cinset (metafunction->pict mf))))
             "\n" "\n"))))

@(with-rewrites
  (ht-append
   40
   (vl-append
    10
    (language->pict L #:nts '(mod M dep deps defn defns kind dnam mnam ph))
    (language->pict L #:nts '(binding bindings))
    (language->pict L #:nts '(Σ cell)))
   (vl-append
    10
    (language->pict L #:nts '(s-exp nam))
    (language->pict L #:nts '(ast val func var))
    (language->pict L #:nts '(stx id))
    (language->pict L #:nts '(body)))))

@; ----------------------------------------
@(sig (signature cmodule : s-exp M -> M))

@(mf-cases (#t)
           cmodule)

@; ----------------------------------------
@(sig (signature cbody : (Seq body ...) mnam (Seq dep ...) (Seq defn ...) M Σ -> M))

@(mf-cases (#f #t #t #t #t #t #t #t #t #t)
           cbody)

@; ----------------------------------------
@(sig (signature cexpr : stx ph env Σ -> ast))

@(mf-cases (#f #f #t #f #f #f #f #f)
           cexpr)

@; ----------------------------------------
@(sig (signature eval : ast Σ -> val))

@(mf-cases (#f #f #f)
           eval)

@(sig (signature maybe-eval : ph ast Σ -> val))

@(mf-cases (#f #f) 
           maybe-eval)

@; ----------------------------------------
@(sig (signature ast-shift : ast ph -> ast))

@(mf-cases (#f #f #f #f #f #f #f)
           ast-shift)

@; ----------------------------------------

@(sig (signature visit : mnam ph M Σ -> Σ))

@(mf-cases (#f #f) visit)

@(sig (signature visit* : deps ph M Σ -> Σ))

@(mf-cases (#f #f) visit*)

@(sig (signature visit-body : defns nam ph Σ -> Σ))

@(mf-cases (#f #t #t #t) visit-body)

@; ----------------------------------------

@(sig (signature run : mnam ph M Σ -> Σ))

@(mf-cases (#f #f) run)

@(sig (signature run* : deps ph M Σ -> Σ))

@(mf-cases (#f #f) run*)

@(sig (signature run-body : defns nam ph Σ -> Σ))

@(mf-cases (#f #t #t) run-body)

@; ----------------------------------------

@(sig (signature wrap : s-exp bindings -> stx))

@(mf-cases (#f #f) wrap)

@(sig (signature strip : stx -> s-exp))

@(mf-cases (#f #f) strip)

@(sig (signature add-to-stx : stx bindings -> stx))

@(mf-cases (#t #f) add-to-stx)

@(sig (signature add : body bindings -> body))

@(mf-cases (#f) add)

@(sig (signature resolve : id ph -> maybe-binding))

@(mf-cases (#f #f) resolve)


@(sig (signature shift : stx-or-ast ph -> stx-or-ast))

@(mf-cases (#t #f #f) shift)

@; ----------------------------------------

@(sig (signature parse-req : stx ph -> [Tup nam ph]))

@(mf-cases (#f #f #f) parse-req)

@(sig (signature import : mnam ph_Δ M -> bindings))

@(mf-cases (#f) import)
