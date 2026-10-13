
(module utils (lib "slideshow.ss" "texpict")
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "math.ss"))

  ;; defterm
  (define (dt i) (bit i))

  ;; tt typeset  with optional hiliting
  (define prog 
    (case-lambda
     [(s) (prog #f s)]
     [(p s) (prog p s purple)]
     [(p s color)
      (let ([lines (let loop ([s (regexp-replace* (string #\tab) s "        ")])
		     (let ([m (regexp-match (format "([^~a~a]*)[~a~a]+(.*)"
						    #\newline #\return #\newline #\return)
					    s)])
		       (if m
			   (if (string=? "" (cadr m))
			       (loop (caddr m))
			       (cons (cadr m)
				     (loop (caddr m))))
			   (list s))))])
	(apply
	 vl-append
	 (map
	  (lambda (line)
	    (let loop ([line line])
	      (let ([m (and p (regexp-match-positions p line))])
		(if m
		    (hc-append
		     (tt (substring line 0 (caar m)))
		     (colorize (tt (substring line (caar m) (cdar m))) color)
		     (loop (substring line (cdar m) (string-length line))))
		    (tt line)))))
	  lines)))]))

  (define (cat l)
    (let loop ([l l])
      (if (null? (cdr l))
	  (car l)
	  (string-append (car l) "\n" (loop (cdr l))))))
  
  (define prog* (lambda l (prog (cat l))))
  (define prog$* (lambda (p . l) (prog p (cat l))))

  (define (code/patterns patterns s)
    (let ([l (string-length s)])
      (let loop ([p 0])
	(if (= p l)
	    (blank)
	    (let ploop ([patterns patterns])
	      (cond
	       [(null? patterns) (error 'code "can't match: ~a" (substring s p l))]
	       [(regexp-match-positions (car patterns) s p)
		=> (lambda (m)
		     (hbl-append
		      ((cadr patterns) (substring s (caar m) (cdar m)))
		      (loop (cdar m))))]
	       [else (ploop (cddr patterns))]))))))

  (define (strike color p)
    (cbl-superimpose
     p
     (linewidth
      3
      (colorize
       (dc (lambda (dc x y)
	     (send dc draw-line 
		   x (+ y (/ (pict-height p) 2))
		   (+ x (pict-width p)) (+ y (/ (pict-height p) 2))))
	   (pict-width p) (pict-height p)
	   (pict-ascent p) (pict-descent p))
       color))))

  (define (background color p)
    (cbl-superimpose
     (colorize
      (dc (lambda (dc x y)
	    (send dc draw-rectangle
		  x y 
		  (pict-width p) (pict-height p)))
	  (pict-width p) (pict-height p)
	  (pict-ascent p) (pict-descent p))
      color)
     p))
  
  (define (symbol n)
    (text (string (integer->char n)) 'symbol font-size))

  (define sym:in (symbol 206))
  (define sym:rightarrow (symbol 174))
  (define sym:infinity (symbol 165))

  (define turnstyle
    (let* ([neq-sym (symbol 185)]
	   [h (pict-width neq-sym)]
	   [w (* 0.8 h)]
	   [d (pict-descent neq-sym)]
	   [e 1]) ; yuck - extra offset determined by hand
      (linewidth 2
		 (picture*
		  w (+ h d) h d
		  `((connect 0 ,(+ d e) 0 ,(+ d e h))
		    (connect 0.5 ,(+ d e (/ h 2)) ,w ,(+ d e (/ h 2))))))))

  (define (infer r . l)
    (vc-append
     line-sep
     (apply vc-append
	    line-sep
	    l)
     (hline (+ font-size (apply max (map pict-width (cons r l)))) 1)
     r))

  (define (ante-append . l)
    (apply hbl-append (* 3 font-size) l))
  (define (ante-append/close . l)
    (apply hbl-append (* 1.5 font-size) l))

  (provide pi dt
	   prog prog* prog$*
	   code/patterns
	   strike background

	   symbol
	   sym:in sym:rightarrow sym:infinity

	   turnstyle

	   infer
	   ante-append
	   ante-append/close))
