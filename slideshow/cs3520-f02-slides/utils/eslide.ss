
(module eslide (lib "slideshow.ss" "texpict")

  (require "colors.ss"
	   "utils.ss"
	   "alg.ss"
	   "env.ss")

  (provide half-page-para
	   quarter-page-para
	   eslide-title
	   eslide
	   pt*
	   pt
	   point
	   no-point
	   key-point)

  (define (half-page-para . l)
    (apply para (/ (pict-width titleless-page) 2) l))

  (define (quarter-page-para . l)
    (rt-superimpose
     (apply para (/ (pict-width titleless-page) 4) l)
     (half-page-para "")))

  (define eslide-title (make-parameter "<No Title>"))

  (define (eslide l r . rest)
    (slide/title/tall
     (eslide-title)
     (rb-superimpose
      l
      (apply half-page-para rest) )
     (page-para r)))

  (define (pt* s) (alg-code s))
  (define (pt s) (alg-code s))

  (define point
    (let ([wrap (lambda (x)
		  (if (env? x)
		      x
		      (make-env x x null)))])
      (case-lambda
       [(p d) (show-env (wrap p) (if (env? d) (env-frame d) d))]
       [(p) (show-env (wrap p) (if (env? p) (env-frame p) p))])))

  (define (no-point p) (show-env (if (env? p) p (make-env p p null)) #f))

  (define (key-point page-para . l)
    (apply
     page-para
     (colorize (arrowhead (* font-size 3/4) 0) RedColor)
     l)))

  
