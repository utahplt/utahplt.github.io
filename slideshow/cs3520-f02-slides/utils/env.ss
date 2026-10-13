

(module env (lib "slideshow.ss" "texpict")
  (require "colors.ss" 
	   "utils.ss" 
	   "alg.ss"
	   (lib "etc.ss"))

  (provide empty-env
	   env-extension
	   closure-env
	   add-binding
	   add-reference-binding
	   show-env
	   (struct env (pict frame refs))
	   (rename array env-array))

  (define ttext alg-code)

  (define-struct env (pict frame refs))

  (define ebullet (cc-superimpose (disk (/ font-size 2))
				  (blank 0 (pict-height (ttext " ")))))

  (define (col-frame p color)
    (cc-superimpose
     p
     (linewidth 2 (colorize (frame (ghost p)) color))))

  (define (array color . l)
    (let ([cells (let ([maxh (apply max (map pict-height l))])
		   (map
		    (lambda (p)
		      (col-frame (inset (cc-superimpose p (blank 0 maxh)) 4 2) color))
		    l))])
      (apply
       hc-append
       -1
       cells)))

  (define arrow-size (quotient font-size 2))

  (define (eframe names vals)
    (apply
     vc-append
     -1
     (let* ([names (map ttext names)]
	    [vals (map (lambda (x) (if (pict? x) x (colorize (ttext x) GreenColor))) vals)]
	    [big-name (ghost (launder (apply cc-superimpose names)))]
	    [big-val (ghost (launder (apply cc-superimpose vals)))])
       (map
	(lambda (name val)
	  (array
	   BlueColor
	   (cc-superimpose name big-name)
	   (cc-superimpose val big-val)))
	names vals))))

  ;; env-extension : list-of-str list-of-str/pict/pair bool env -> env
  ;; vertical extension of an environment; new frame is left-aligned
  ;;  pair vals indicate recursively-extended-env; don't used wth refs? = #t
  (define env-extension
    (opt-lambda (names vals refs? old-env [old-frame (if (env? old-env) 
							 (env-frame old-env)
							 old-env)])
      (let* ([ref-bullets (and refs? (map (lambda (x) (colorize (launder ebullet) RedColor)) vals))]
	     [f (let ([g (eframe names (or ref-bullets (if (pair? (car vals))
							   (map car vals)
							   vals)))])
		  (if (pair? (car vals))
		      (let* ([bodies (map cdr vals)]
			     [big-body (ghost (launder (apply cc-superimpose bodies)))])
			(col-frame
			 (inset
			  (hc-append
			   -1
			   g
			   (apply
			    vc-append
			    -1
			    (map (lambda (body)
				   (array
				    BlueColor
				    (lc-superimpose body big-body)))
				 bodies)))
			  4)
			 BlueColor))
		      g))])
	(let-values ([(refarray refcells)
		      (if refs?
			  (let* ([vals (map (lambda (x) (if (string? x) (colorize (ttext x) GreenColor) x)) vals)]
				 [cells (let ([maxw (apply max (map pict-width vals))])
					  (map
					   (lambda (p)
					     (col-frame (inset (cc-superimpose p (blank maxw 0)) 4 2) RedColor))
					   vals))])
			    (values (apply
				     vc-append
				     -1
				     cells)
				    cells))
			  (values #f #f))])
	  (make-env
	   (vl-append
	    (if (env? old-env) (env-pict old-env) old-env)
	    (let-values ([(x y) (find-lb (if (env? old-env) (env-pict old-env) old-env) old-frame)])
	      (cb-superimpose (colorize (inset (arrow-line 0 (+ y font-size) arrow-size) 0 (- y) 0 0) BlueColor)
			      (blank font-size)))
	    (if refs?
		(let ([together (hc-append
				 font-size
				 f
				 refarray)])
		  (cc-superimpose
		   together
		   (colorize
		    (cons-picture
		     (ghost together)
		     (map
		      (lambda (b c)
			(let-values ([(br bc) (find-rc together b)]
				     [(cl cc) (find-lc together c)])
			  `(place ,br ,bc ,(colorize (arrow-line (- cl br) (- cc bc) arrow-size) RedColor))))
		      ref-bullets refcells))
		    BlueColor)))
		f))
	   f
	   refcells)))))

  (define empty-env (let ([p (linewidth 2 (colorize (circle font-size) BlueColor))])
		      (make-env
		       p
		       p
		       null)))

  (define (closure-env clos-only? indent base-env-pict base-frame clos-env-pict name arg body w/ref?)
    (let* ([bullet (colorize (launder ebullet) (if w/ref? RedColor GreenColor))]
	   [bullet2 (colorize (launder ebullet) BlueColor)]
	   [bullet3 (colorize (launder ebullet) GreenColor)]
	   [frame (eframe (list name) (list bullet))]
	   [ref (and w/ref? (array RedColor bullet3))]
	   [closure (apply array GreenColor
			   (append
			    (if arg
				(list (ttext arg))
				null)
			    (list
			     (ttext body)
			     bullet2)))]
	   [together (let* ([env (vl-append
				  font-size
				  base-env-pict
				  (hbl-append font-size 
					      (inset ((if clos-only? ghost values) frame)
						     (* font-size indent) 0 0 0)
					      (if w/ref?
						  (hbl-append
						   font-size
						   ((if clos-only? ghost values) ref)
						   closure)
						  closure)))])
		       (if clos-only?
			   env
			   (let-values ([(fc ft) (let-values ([(fl ft) (find-lt env frame)])
						   ;; Don't actually use center for fc; instead
						   ;; shift from the left to match the result
						   ;; of env-extension
						   (values (+ fl (/ font-size 2)) ft))]
					[(bc bb) (find-cb env base-frame)])
			     (cons-picture
			      env
			      `((place ,fc ,ft ,(colorize (arrow-line (- bc fc) (- bb ft) 
								      arrow-size) 
							  BlueColor)))))))])
      (let-values ([(br bc) (find-rc together bullet)]
		   [(cl cc) (find-lc together (if w/ref? ref closure))]
		   [(b2c b2t) ((if (eq? clos-env-pict 'self) find-cb find-ct) together bullet2)]
		   [(er eb) (find-rb together (if (eq? clos-env-pict 'self)
						  frame
						  clos-env-pict))])
	(make-env
	 (cc-superimpose
	  together
	  (colorize
	   (cons-picture
	    (if clos-only?
		(ghost together)
		(let ([t (colorize
			  (cons-picture
			   (ghost together)
			   `((place ,br ,bc ,(colorize (arrow-line (- cl br) (- cc bc) arrow-size) 
						       (if w/ref? RedColor GreenColor)))))
			  BlueColor)])
		  (cc-superimpose
		   t
		   (colorize
		    (cons-picture
		     (ghost t)
		     (if ref
			 (let-values ([(rr rc) (find-rc together bullet3)]
				      [(cl cc) (find-lc together closure)])
			   
			   `((place ,rr ,rc ,(arrow-line (- cl rr) (- cc rc) arrow-size))))
			 '()))
		    GreenColor))))
	    `((place ,(+ er font-size) ,(- eb 5) ,(arrow-line (- font-size) 5 arrow-size))
	      (curve ,(+ er font-size) ,(- eb 5) ,b2c ,b2t ,b2c ,(- eb (if (eq? clos-env-pict 'self) 15 5)))))
	   BlueColor))
	 frame
	 (list ref)))))

  ;; Similar to env-extension, but frame goes to the right
  (define (add-binding indent vert? name val ref? base-env-pict var-frame)
    (let*-values ([(bullet) (launder ebullet)]
		  [(ref) (and ref? (array RedColor val))]
		  [(x-frame0) (eframe (list name) (list (if ref? (colorize bullet RedColor) val)))]
		  [(x-frame) (let ([f x-frame0])
			       (if ref?
				   (let ([p (hc-append
					     font-size
					     f
					     ref)])
				     (cc-superimpose
				      p
				      (colorize
				       (cons-picture
					(ghost p)
					(let-values ([(fr fc) (find-rc p f)]
						     [(rl rc) (find-lc p ref)])
					  `((place ,fr ,fc ,(arrow-line (- rl fr) (- rc fc) arrow-size)))))
				       RedColor)))
				   f))]
		  [(x-env) (let ([e (if vert?
					(vl-append
					 font-size
					 base-env-pict
					 (inset x-frame indent 0 0 0))
					(let-values ([(er ec) (find-rc base-env-pict var-frame)])
					  (lb-superimpose
					   (inset base-env-pict 0 0 0 font-size)
					   (inset x-frame (+ font-size er) 0 0 0))))])
			     (let-values ([(xc xt) ((if vert? find-ct find-lc) e x-frame0)]
					  [(ec eb) ((if vert? find-cb find-rc) e var-frame)])
			       (cons-picture
				e
				`((place ,xc ,xt ,(colorize 
						   (arrow-line (- ec xc) (- eb xt) arrow-size) 
						   BlueColor))))))])
      (make-env x-env
		x-frame
		(list ref))))

  ;; ref should be an existing cell (a pict)
  (define (add-reference-binding name ref base-env-pict var-frame)
    (let* ([x-bullet (launder bullet)]
	   [x-env (add-binding #t name x-bullet #f base-env-pict var-frame)])
      (make-env
       (cc-superimpose
	(env-pict x-env)
	(colorize
	 (cons-picture
	  (ghost (env-pict x-env))
	  (let-values ([(yr yc) (find-rc (env-pict x-env) ref)]
		       [(xr xc) (find-rc (env-pict x-env) x-bullet)]
		       [(fs) (* 2 font-size)])
	    `((place ,(+ yr arrow-size) ,yc ,(arrow-line (- arrow-size) 0 arrow-size))
	      (curve ,xr ,xc ,(+ xr fs) ,(/ (+ xc yc) 2) ,(+ xr fs) ,xc)
	      (curve ,yr ,yc ,(+ xr fs) ,(/ (+ xc yc) 2) ,(+ xr fs) ,yc))))
	 BlueColor))
       (env-frame x-env)
       (list ref))))

  (define current-arrow (colorize (arrow font-size 0) PurpleColor))

  (define (show-env env current-frame)
    (let ([p (lt-superimpose
	      (blank (pict-width titleless-page)
		     (* 0.60 (pict-height titleless-page)))
	      (inset (env-pict env) (+ (pict-width current-arrow) line-sep) 0 0 0))])
      (if current-frame
	  (let-values ([(x y) (find-lb p current-frame)])
	    (cons-picture
	     p
	     `((place ,(- x (pict-width current-arrow) line-sep) ,y ,current-arrow))))
	  p))))
