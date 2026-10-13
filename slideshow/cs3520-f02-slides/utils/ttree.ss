(module ttree (lib "slideshow.ss" "texpict")

  (require "colors.ss"
	   "utils.ss"
	   "alg.ss"
	   (lib "list.ss"))

  (provide type-tree)

  (define (type-tree prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append)
    (let ([prog (lambda (s)
		  (cond
		   [(string? s) (alg-code s)]
		   [s s]
		   [else #f]))])
      (let ([pre (prog pre)]
	    [prepre (prog prepre)]
	    [a (prog a)]
	    [sep (prog (or sep ""))]
	    [b (and b (prog b))]
	    [sep2 (prog (or sep2 ""))]
	    [c (and c (prog c))]
	    [post (prog post)]
	    [at atype]
	    [atype (if atype (prog atype) (blank))]
	    [bt btype]
	    [btype (if btype (prog btype) (blank))]
	    [ct ctype]
	    [ctype (and ctype (prog ctype))]
	    [et etype]
	    [etype (if (pict? etype) etype (if etype (prog etype) (blank)))]
	    [wtype (and wtype (prog wtype))])
	(let* ([expr (hbl-append pre a sep (or b (blank)) (or sep2 (blank)) (or c (blank)) post)]
	       [expr+ (hb-append prepre expr)]
	       [types (let ([ab (if bt
				    (hbl-append (* 3 font-size) atype btype)
				    atype)])
			(if ctype
			    (hbl-append (* 3 font-size) ab ctype)
			    ab))])
	  (let ([whole (let* ([sep (/ font-size 2)]
			      [p (vc-append sep
					    (tv-append sep
						       expr+
						       types)
					    etype)])
			 (if wtype 
			     (vc-append sep p wtype)
			     p))])
	    (let ([base (ghost whole)]
		  [show (lambda (whole p)
			  (let-values ([(x y) (find-lb whole p)])
			    (cons-picture
			     whole
			     `((place ,x ,y ,p)))))]
		  [tline (lambda (whole e t dy l?)
			   (let-values ([(ec eb) (find-cb whole e)]
					[(el eb_) (find-lb whole e)]
					[(tc tt) ((if l? find-lt find-ct) whole t)]
					[(w/2) (/ (pict-width e) 2)])
			     (cons-picture
			      whole
			      `((connect ,tc ,tt ,(if l? (+ el 5) ec) ,(+ eb dy))
				(connect ,(- ec w/2) ,(+ eb dy) ,(+ ec w/2) ,(+ eb dy))))))])
	      (let* ([start (show (show base expr) prepre)]
		     [atype (if at
				(tline (show start atype)
				       a atype 0 #f)
				start)]
		     [btype (if bt
				(tline (show atype btype)
				       b btype 0 #f)
				atype)]
		     [ctype (if ctype
				(tline (show btype ctype)
				       c ctype 0 #f)
				btype)]
		     [all (if et
			      (tline (show ctype etype)
				     expr etype -5 #f)
			      ctype)])
		(map list
		     (append
		      (list start)
		      (if at (list atype) null)
		      (if bt (list btype) null)
		      (if ct (list ctype) null)
		      (if et (list all) null)
		      (if wtype 
			  (list (tline (show all wtype) expr+ wtype -10 #t)) 
			  null)))))))))))
