#cs
(module utils (lib "slideshow.ss" "slideshow")
  (require "colors.ss"
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss"))

  ;; defterm
  (define (dt i) (bit i))
  
  (define (symbol n)
    (text (string (integer->char n)) 'symbol font-size))

  (define sym:in (symbol 206))
  (define sym:rightarrow (symbol 174))
  (define sym:infinity (symbol 165))
  (define sym:times (symbol 180))
  (define sym:implies (symbol 222))
  (define sym:emdash (symbol 190))
  
  (define recipe-item
    (opt-lambda (name [hilite #f])
      (let* ([p (page-para (if (pict? name)
			       name
			       (bt name)))]
	     [b (colorize (filled-rectangle (pict-width p) (pict-height p))
			  (cond
			   [(string? hilite) hilite]
			   [(hilite . is-a? . color%) hilite]
			   [hilite
			    (make-object color% #xff #xff #x00)]
			   [else
			    (make-object color% #xcc #xcc #xff)]))])
	(lc-superimpose b p))))
      
  (define (recipe-desc . l)
    (apply page-item l))

  (define (strike-through p)
    (cc-superimpose
     p
     (colorize (linewidth 2 (hline (pict-width p) (pict-height p))) RedColor)))

  (define (bkbox p color)
    (drop
     (cc-superimpose
      (colorize
       (filled-rectangle (pict-width p) (pict-height p))
       color)
      p)
     (pict-descent p)))
  
  (define (problem . l)
    (with-font 'roman (lambda () (apply page-item l))))

  (define add-gp-arrow
    (opt-lambda (p fxr fyr txr tyr [to-left? #f])
      (let* ([w (pict-width p)]
	     [h (pict-height p)]
	     [fx (* w fxr)]
	     [fy (* h fyr)]
	     [tx (* w txr)]
	     [ty (* h tyr)]
	     [cx (* w (if to-left?
			  (/ (min fxr txr) 2)
			  (- 1 (/ (- 1 (max fxr txr)) 2))))]
	     [cy (* h (/ (+ fyr tyr) 2))])
	(lt-superimpose
	 p
	 (colorize (inset (arrowhead gap-size (atan (- (- ty cy))
						    (- tx cx)))
			  (- tx (/ gap-size 2))
			  (- ty (/ gap-size 2))
			  0 0)
		   "orange")
	 (colorize
	  (linewidth
	   3
	   (dc (lambda (dc x y)
		 (send dc draw-spline 
		       (+ x fx) (+ y fy)
		       (+ x cx) (+ y cy)
		       (+ x tx) (+ y ty)))
	       w h 0 0))
	  "orange")))))
  
  (define (in-aq p)
    (let ([w (+ (pict-width p) gap-size gap-size)]
          [h (+ (pict-height p) gap-size gap-size)])
      (lb-superimpose
       (colorize (filled-rectangle w h) "sky blue")
       (colorize (rectangle w h) "blue")
       (inset p gap-size 0 0 gap-size))))
  
  (provide dt symbol
	   sym:in sym:rightarrow sym:infinity sym:times sym:implies sym:emdash
	   recipe-item recipe-desc
	   strike-through
	   problem bkbox add-gp-arrow
	   in-aq))
