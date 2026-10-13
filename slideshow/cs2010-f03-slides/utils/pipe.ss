#cs
(module pipe (lib "slideshow.ss" "slideshow")
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  ;; Remarkably, this file contains the only code that doesn't run
  ;;  in both v20x and v30x. So we make it work...
  (define-values (make-pixels pixels-ref pixels-set! pixel->integer integer->pixel)
    (apply
     values
     (map (lambda (s)
	    (dynamic-require 'mzscheme s))
	  (if (identifier-binding #'make-bytes)
	      '(make-bytes bytes-ref bytes-set! values values)
	      '(make-string string-ref string-set! char->integer integer->char)))))

  (define (shade bm rx gx bx)
    (let* ([w (send bm get-width)]
           [h (send bm get-height)]
           [dc (make-object bitmap-dc% bm)]
           [scale (lambda (x c)
                    (integer->pixel
                     (inexact->exact (floor (* x (pixel->integer c))))))])
      (let ([s (make-pixels (* w h 4))])
        (send dc get-argb-pixels 0 0 w h s)
        (let loop ([pos (* w h 4)])
          (let ([pos (- pos 4)])
            (pixels-set! s (+ pos 1) (scale rx (pixels-ref s (+ 1 pos))))
            (pixels-set! s (+ pos 2) (scale gx (pixels-ref s (+ 2 pos))))
            (pixels-set! s (+ pos 3) (scale bx (pixels-ref s (+ 3 pos))))
            (unless (zero? pos)
              (loop pos))))
        (send dc set-argb-pixels 0 0 w h s))
      (send dc set-bitmap #f)
      (bitmap bm)))
  
  (define (make-bm file r g b l t bt rt)
    (define i #f)
    (lambda ()
      (or i
	  (let* ([path (build-path (this-expression-source-directory) file)]
		 [bm (make-object bitmap% path 'unknown/mask)])
	    (set! i
		  (inset
		   (if (= 1 r g b)
		       (bitmap bm)
		       (shade bm r g b))
		   l t bt rt))
	    i))))

  (define (shade-copper f t l b r)
    (make-bm f 1 0.8 0 t l b r))
  (define (shade-lead f t l b r)
    (make-bm f 0.6 0.6 0.6 t l b r))
  (define (shade-plain f t l b r)
    (make-bm f 1 1 1 t l b r))

  (define (straight shade)
    (shade "straight.png" 0 32 0 20))
  (define (fork shade)
    (let ([branch (shade "branch.png" 0 32 0 0)]
	  [vert (shade "vertical.png" 5 0 -5 0)]
	  [ell (shade "ell.png" 34 0 0 18)])
      (opt-lambda ([skip 0])
	(vl-append
	 (branch)
	 (let loop ([skip skip])
	   (if (zero? skip)
	       (ell)
	       (vl-append
		(vert)
		(loop (sub1 skip)))))))))
  (define (faucet shade)
    (shade "faucet.png" 0 0 0 0))

  (define (no-pipe)
    (blank 98 99))

  (define water-img #f)

  (define (water)
    (unless water-img
      (set! water-img (inset (bitmap (build-path (this-expression-source-directory)  "drop.png"))
			     64 0 0 54)))
    water-img)

  (define plain-straight (straight shade-plain))
  (define plain-fork (fork shade-plain))
  (define plain-faucet (faucet shade-plain))

  (define lead-straight (straight shade-lead))
  (define lead-fork (fork shade-lead))
  (define lead-faucet (faucet shade-lead))

  (define copper-straight (straight shade-copper))
  (define copper-fork (fork shade-copper))
  (define copper-faucet (faucet shade-copper))

  (provide plain-straight plain-fork plain-faucet
	   lead-straight lead-fork lead-faucet
	   copper-straight copper-fork copper-faucet
	   water no-pipe))


  