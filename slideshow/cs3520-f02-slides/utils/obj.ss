
(module obj (lib "slideshow.ss" "texpict")

  (require "colors.ss"
	   "utils.ss"
	   "alg.ss")

  (provide mk-class
	   mk-tree
	   tree tree2 tree-types
	   mk-part mk-parts
           mk-flat-obj
	   
	   array dash-placeholder dash-box)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (vl-append
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
	       methods))))))

  '(define (tree class meth)
    (let ([xtt (lambda (c m)
		 (if (and (equal? c class)
			  (equal? m meth))
		     (colorize (alg-code m) purple)
		     (alg-code m)))])
      (define fish-class
	(mk-class
	 (alg-code "fish")
	 (list (alg-code "size"))
	 (list (xtt "fish" "initialize")
	       (xtt "fish" "get__size")
	       (xtt "fish" "grow")
	       (xtt "fish" "eat"))))
      (define colorfish-class
	(mk-class
	 (alg-code "colorfish")
	 (list (alg-code "color"))
	 (list (xtt "colorfish" "set__color")
	       (xtt "colorfish" "get__color"))))
      (define pickyfish-class
	(mk-class
	 (alg-code "pickyfish")
	 null
	 (list (xtt "pickyfish" "grow"))))

      (vc-append
       font-size
       (let ([t (vc-append
		 (* 2 font-size)
		 fish-class
		 (ht-append
		  (* 2 font-size)
		  colorfish-class
		  pickyfish-class))])
	 (add-line 
	  (add-line 
	   t 
	   colorfish-class find-ct
	   fish-class find-cb 
	   2)
	  pickyfish-class find-ct
	  fish-class find-cb
	  2))
       (let ([c class][m meth])
	 (cond
	  [(and (equal? c "fish")
		(equal? m "grow"))
	   (alg-code*
	    "grow(f)"
	    "set size=+(size,f)")]
	  [(and (equal? c "fish")
		(equal? m "eat"))
	   (alg-code*
	    "eat(o) let s = send o get__size()"
	    " in send self grow(s)")]
	  [(and (equal? c "fish")
		(equal? m "get__size"))
	   (alg-code "get__size() size")]
	  [(and (equal? c "pickyfish")
		(equal? m "grow"))
	   (alg-code*
	    "grow(f)"
	    "super grow(-(f, 1))")]
	  [else (blank)])))))


  (define (mk-tree big? types? class meth)
    (let ([xtt (lambda (c rt m argt flds super)
		 (let ([mn (if (and (equal? c class)
				    (equal? m meth))
			       (colorize (alg-code m) purple)
			       (alg-code m))])
		   (cond
		    [big?
		     (hbl-append
		      mn
		      (t ", {")
		      (apply hbl-append
			     (/ font-size 2)
			     flds)
		      (t "}, ")
		      (colorize (alg-code super) blue))]
		    [types?
		     (hbl-append (alg-code rt)
				 (alg-code " ")
				 mn
				 (alg-code argt))]
		    [else
		     mn])))])
      (define fish-fields (list (alg-code (if types? "intTd size" "size"))))
      (define fish-grow-method 
	(xtt "fish" "void" "grow" "(intTd)" fish-fields "object"))
      (define fish-methods 
	(list (xtt "fish" "voidTd" "initialize" "(intTd)" fish-fields "object")
	      (xtt "fish" "intTd" "get__size" "()" fish-fields "object")
	      fish-grow-method 
	      (xtt "fish" "voidTd" "eat" "(fish)" fish-fields "object")))
      (define fish-class
	(mk-class
	 (alg-code "fish")
	 fish-fields
	 fish-methods))
      (define colorfish-fields
	(append
	 (if big? fish-fields null)
	 (list (alg-code (if types? "intTd color" "color")))))
      (define colorfish-class
	(mk-class
	 (alg-code "colorfish")
	 colorfish-fields
	 (append
	  (if big? fish-methods null)
	  (list (xtt "colorfish" "void" "set__color" "(intTd)" colorfish-fields "fish")
		(xtt "colorfish" "int" "get__color" "()" colorfish-fields "fish")))))
      (define pickyfish-fields (if big? fish-fields null))
      (define pickyfish-class
	(mk-class
	 (alg-code "pickyfish")
	 pickyfish-fields
	 (let ([pf (xtt "pickyfish" "void" "grow" "(intTd)" pickyfish-fields "fish")])
	   (if big?
	       (map
		(lambda (p)
		  (if (eq? p fish-grow-method )
		      pf
		      p))
		fish-methods)
	       (list pf)))))

      (vc-append
       font-size
       (let ([t (vc-append
		 (* 2 font-size)
		 fish-class
		 (ht-append
		  (* 2 font-size)
		  colorfish-class
		  (if (and meth (not class))
		      (vl-append font-size
				 pickyfish-class
				 meth)
		      pickyfish-class)))])
	 (add-line 
	  (add-line 
	   t 
	   colorfish-class find-ct
	   fish-class find-cb 
	   2)
	  pickyfish-class find-ct
	  fish-class find-cb
	  2))
       (let ([c class][m meth])
	 (cond
	  [(and (equal? c "fish")
		(equal? m "grow"))
	   (alg-code*
	    "grow(f)"
	    "set size=+(size,f)")]
	  [(and (equal? c "fish")
		(equal? m "eat"))
	   (alg-code*
	    "eat(o) let s = send o get__size()"
	    "in send self grow(s)")]
	  [(and (equal? c "fish")
		(equal? m "get__size"))
	   (alg-code "get__size() size")]
	  [(and (equal? c "pickyfish")
		(equal? m "grow"))
	   (alg-code*
	    "grow(f)"
	    "super grow(-(f, 1))")]
	  [(and (equal? c "colorfish")
		(equal? m "set__color"))
	   (alg-code*
	    "set__color(c)"
	    "set color = c")]
	  [else (blank)])))))

  (define (tree class meth)
    (mk-tree #f #f class meth))
  (define (tree2 class meth)
    (mk-tree #t #f class meth))
  (define (tree-types class meth)
    (mk-tree #f #t class meth))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (mk-obj round? class fields)
    (let ([fields
	   (if (pict? fields)
	       fields
	       (table
		3
		(apply
		 append
		 (map
		  (lambda (x)
		    (list (car x) (tt "=") (cadr x)))
		  fields))
		lc-superimpose lc-superimpose
		font-size line-sep))])
      (let ([w (max (pict-width class) (pict-width fields))])
	(let ([p (vc-append
		  (* line-sep 2)
		  (colorize class blue)
		  (colorize (linewidth 2 (hline (+ w (* 4 line-sep)) 1))
			    GreenColor)
		  fields)])
	  (if round?
	      (color-round-frame p (/ font-size 2) GreenColor 2)
	      (color-frame p GreenColor 2))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (tree-append c m . l)
    (let ([h (pict-height titleless-page)])
      (cc-superimpose
       (lt-superimpose
	titleless-page
	(tree c m)
	(inset
	 (apply vl-append (* 2 font-size) l)
	 (+ (* 1/2 client-w) font-size)
	 0 0 0))
       (vline 0 h))))

  (define (objname s) (it s))
  (define (name-fish name fish)
    (ht-append
     (objname name)
     (tt " = ")
     fish))

  (define dash-placeholder (ghost (alg-code "0")))
  (define dash-box (color-dash-frame
		    (inset dash-placeholder 4 2)
		    (/ font-size 4)
		    blue 2))

  (define (array color . l) 
    (let ([cells (let ([maxh (apply max (map pict-height l))])
		   (map
		    (lambda (p)
		      (if (eq? p dash-placeholder)
			  dash-box
			  (color-frame
			   (inset (cc-superimpose p (blank 0 maxh))
				  4 2)
			   color 2)))
		    l))])
      (apply
       hc-append
       -1
       cells)))

  (define (vector-box . l) 
    (inset (apply array GreenColor l) (* 2 line-sep)))

  (define (mk-flat-obj round? cname fields)
    (mk-obj round?
	    (tt cname) 
	    (if (null? fields)
		(vector-box (blank 0 (pict-height (alg-code "0"))))
		(apply vector-box (map (lambda (x) 
					 (if (pict? x) x (tt x)))
				       fields)))))
  
  (define (mk-part cname fields)
    (mk-flat-obj #t cname fields))
  
  (define (mk-parts parts)
    (color-frame
     (inset
      (apply
       vl-append
       (/ font-size 2)
       (map
	(lambda (p)
	  (if (pict? p)
	      p
	      (mk-part (car p) (cdr p))))
	parts))
      (/ font-size 2))
     GreenColor 2)))


