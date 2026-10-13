#lang slideshow
(require redex
         "model.rkt"
         "model-pict.rkt")

;; For quick checking of rendering...

(slide
 (scale
  (module-grammar-pict)
  2))


  #;
(with-rewrites
   (vl-append
    10
    (metafunction->pict run)
    (metafunction->pict run*)
    (parameterize ([linebreaks '(#f #t #t)])
      (metafunction->pict run-body))))
#;
(slide
 (with-rewrites
  (language->pict L #:nts '(s-exp nam)))
 (with-rewrites
  (language->pict L #:nts '(mod mods dep deps defn ph)))
 (with-rewrites
  (parameterize ([linebreaks '(#t)])
    (metafunction->pict cmodule)))
 (with-rewrites
  (parameterize ([metafunction-cases '(0)]
                 [linebreaks '(#t)])
    (metafunction->pict cbody)))
 (with-rewrites
  (parameterize ([metafunction-cases '(9)]
                 [linebreaks '(#t)])
    (metafunction->pict cbody))))

