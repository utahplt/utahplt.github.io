#cs
(module obj (lib "slideshow.ss" "slideshow")

  (require "colors.ss"
	   "utils.ss"
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide mk-class
	   tree-append
	   tree-append/keep-root
	   itree-append
	   itree-append/keep-root
	   treeline
	   itreeline)

  (define bg-color (make-object color% 240 240 192))

  (define (mk-class name fields methods)
    (let* ([n (blank
	       (apply
		max
		(map
		 pict-width
		 (append
		  (list name)
		  fields
		  methods)))
	       0)]
	   [f (lambda (p)
		(color-frame (inset (lc-superimpose n p)
				    (* 2 line-sep))
			     blue 2))])
      (let ([p (vl-append
		-1
		(colorize (f (cc-superimpose n name))
			  blue)
		(if (null? fields)
		    (blank 1 1)
		    (f (apply
			vl-append
			line-sep
			fields)))
		(if (null? methods)
		    (blank 1 1)
		    (f (apply
			vl-append
			line-sep
			methods))))])
	(cc-superimpose
	 (colorize (filled-rectangle (pict-width p) (pict-height p)) bg-color)
	 p))))

  (define (treeline p)
    (linewidth 1 (colorize p BlueColor)))

  (define (itreeline p)
    (linewidth 1 (colorize p OrangeColor)))

  (define (sht-append a b) (ht-append b a))

  (define-struct tree (root body))

  (define (tree-append*/keep-root treeline l? r? t . r)
    (let ([body (apply tree-append* treeline t r)])
      (make-tree t
		 (let-values ([(x y) (find-lt body t)])
		   (inset body
			  (if l? 0 (- x)) 
			  0 
			  (if r? 0 (- (- (pict-width body) (pict-width t) x)))
			  0)))))

  (define (tree-append/keep-root l? r? t . r)
    (apply tree-append*/keep-root treeline l? r? t r))

  (define (itree-append/keep-root l? r? t . r)
    (apply tree-append*/keep-root itreeline l? r? t r))

  (define (tree->pict p)
    (if (tree? p)
	(tree-body p)
	p))

  (define (tree->root-pict p)
    (if (tree? p)
	(tree-root p)
	p))

  (define (tree-inset p l)
    (if (tree? p)
	(let-values ([(x y) (find-lt (tree-body p) (tree-root p))])
	  (inset l
		 x 0 
		 (- (pict-width (tree-body p)) (pict-width (tree-root p)) x) 0))
	l))

  (define (tree-insets p)
    (let-values ([(x y) (if (tree? p)
			    (find-lt (tree-body p) (tree-root p))
			    (values 0 0))])
      (values (+ x (/ (pict-width (tree->root-pict p)) 2))
	      (+ (/ (pict-width (tree->root-pict p)) 2) 
		 (- (pict-width (tree->pict p)) (pict-width (tree->root-pict p)) x)))))

  (define (tree-append* treeline t . r)
    (let-values ([(h)
		  (let loop ([l r])
		    (cond
		     [(null? l) (blank)]
		     [(null? (cdr l))
		      (vc-append
		       (tree-inset (car l) (treeline (vline 0 gap-size)))
		       (tree->pict (car l)))]
		     [(eq? 'nothing (car l))
		      (loop (cdr l))]
		     [(eq? 'gap (car l))
		      (inset (loop (cdr l)) gap-size 0 0 0)]
		     [(memq (car l) '(down down-left down-right))
		      (let ([l (cdr l)]
			    [mode (car l)]
			    [p (tree->pict (cadr l))])
			(ht-append
			 gap-size
			 (let ([h (apply max
					 (map (lambda (p)
						(cond
						 [(pict? p) (pict-height p)]
						 [(tree? p) (pict-height (tree-body p))]
						 [else 0]))
					      r))]
			       [w (/ (pict-width p) 2)])
			   (cond
			    [(eq? mode 'down)
			     (inset (vc-append
				     (treeline (vline 0 (+ h (* 2 gap-size))))
				     p)
				    (- w) 0 (- (- (pict-width p) w)) 0)]
			    [else
			     (let ([p ((if (eq? mode 'down-right) vl-append vr-append)
				       (treeline 
					((if (eq? mode 'down-right) vl-append vr-append)
					 (vline 0 (+ h (* 3/2 gap-size)))
					 ((if (eq? mode 'down-right) ht-append sht-append)
					  (hline (+ w (/ gap-size 2)) 0)
					  (vline 0 (/ gap-size 2)))))
				       (inset p (* 1/2 gap-size) 0 0 0))])
			       (if (eq? mode 'down-right) 
				   (inset p 0 0 (- (pict-width p)) 0)
				   (inset p (- (pict-width p)) 0 0 0)))]))
			 (loop (cdr l))))]
		     [else
		      (ht-append
		       gap-size
		       (let ([p (car l)])
			 (vc-append
			  (tree-inset p (treeline (vline 0 gap-size)))
			  (if (tree? p)
			      (tree->pict p)
			      p)))
		       (loop (cdr l)))]))]
		 [(lw __1) (if (null? r) (values 0 0) (tree-insets (car r)))]
		 [(__2 rw) (if (null? r) (values 0 0) (tree-insets (car (last-pair r))))])
      (vc-append
       (inset
	(vc-append
	 t
	 (treeline (vline 0 gap-size))
	 (treeline
	  (dc (lambda (dc x y)
		(send dc draw-line x (+ y gap-size) (+ x gap-size) y)
		(send dc draw-line (+ x (* 2 gap-size)) (+ y gap-size) (+ x gap-size) y)
		(send dc draw-line (+ x (* 2 gap-size)) (+ y gap-size) x (+ y gap-size)))
	      (* 2 gap-size) gap-size 0 0))
	 (treeline
	  (hline (- (pict-width h) lw rw) 0)))
	lw 0 rw 0)
       h)))

  (define (tree-append t . r)
    (apply tree-append* treeline t r))

  (define (itree-append t . r)
    (apply tree-append* itreeline t r))
     

  )

