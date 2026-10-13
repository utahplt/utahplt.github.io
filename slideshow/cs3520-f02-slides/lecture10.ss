
(module lecture10 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss"
	   (lib "list.ss"))

  (define ContourColor PurpleColor)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title/tall
   "Lexical Addresses"
   (page-para "As we saw in the last lecture, the expression")
   (alg-code* "let x = 1  y = 2"
	      "  in let f = proc (x) +(x, y)"
	      "    in (f x)")
   (page-para "might be compiled to")
   (alg-code* "let _ = 1 _ = 2"
	      "  in let _ = proc (_) +(<0,0>, <1,1>)"
	      "    in (<0,0> <1,0>)")
   (page-para (alg-code "<n, m>") "means:"
	      (alg-code "n") "frames up in the environment,"
	      "at position" (alg-code "m"))
   'next
   (colorize (page-para "How can we compute" (alg-code "<n, m>")
			"for every bound variable without running the code?")
	     BlueColor))


  (slide/title
   "Computing Lexical Addresses"  
   (page-item "What creates a new frame?")
   'next
   (colorize (page-para* (alg-code "let") ", " 
			 (alg-code "letrec")
			 ", and (application of)" (alg-code "proc"))
	     BlueColor)
   'next
   (blank)
   (page-item "So, to compute the" (alg-code "n") "in" (alg-code "<n, m>")
	      ", count the number of enclosing"
	      (hbl-append (alg-code "let") (t ",")) (alg-code "letrec") ", and" (alg-code "proc") 
	      "keywords between the bound variable and its binding")
   (page-item "The" (alg-code "m") "in" (alg-code "<n, m>") "is simply"
	      "the variable's position in its binding set"))

  (define (proc-contour args body)
    (hbl-append
     (alg-code "proc (")
     (let loop ([args args])
       (cond
	[(null? args) (blank)]
	[(null? (cdr args)) (alg-code (car args))]
	[else (hbl-append
	       (alg-code (car args))
	       (alg-code ", ")
	       (loop (cdr args)))]))
     (alg-code ") ")
     (color-frame (inset body 2) ContourColor)))

  (define (let-contour args vals body)
    (let* ([binds (apply vl-append
			 line-sep
			 (map (lambda (x y) 
				(htl-append 
				 (alg-code (format "~a = " x))
				 y))
			      args vals))])
      (vl-append
       line-sep
       (htl-append (alg-code "let ") binds)
       (htl-append (alg-code "  in ")
		   (color-frame (inset body 2) ContourColor)))))
  
  (define (letrec-contour args vals body)
    (let* ([args (map (lambda (x) (alg-code (format "~a = " x))) args)]
	   [code (inset
		  (vl-append
		   line-sep
		   (htl-append (alg-code "letrec ")
			       (apply vl-append
				      line-sep
				      args)
			       (apply vl-append
				      line-sep
				      vals))
		   (htl-append (alg-code "  in ")
			       body))
		  2)]
	   [h (pict-height code)]
	   [w (pict-width code)])
      (let-values ([(l t) (find-lt code (car vals))]
		   [(l2 b) (find-lb code (car (last-pair vals)))])
	(cons-colorized-picture
	 code
	 ContourColor
	 `((connect 0 0 0 ,(+ b 1))
	   (connect 0 ,(+ b 1) ,(- l 1) ,(+ b 1))
	   (connect ,(- l 1) ,(+ b 1) ,(- l 1) ,h)
	   (connect ,(- l 1) ,h ,w ,h)
	   (connect ,w ,h ,w 0)
	   (connect ,w ,0 0 0))))))
			   

  (slide/title
   "Computing Lexical Addresses"  
   (page-para "Visualize as" (dt "countours") "that separate"
	      "environment extension from the expressions that use it")
   (blank)
   'alts
   (list (list (proc-contour '("x") (alg-code "+(x, 7)"))
	       (blank)
	       (page-item "Count contour crossings to get" (alg-code "n + 1"))
	       'next
	       (page-item "Cross 1 contour from bound" (alg-code "x")
			  "to binding" (alg-code "x") ", so first part of address is"
			  (alg-code "0"))
	       (page-item "Full address is" (alg-code "<0, 0>")))
	 (list (proc-contour '("y")
			     (proc-contour '("x" "z") (alg-code "+(x, -(y, z))")))
	       (blank)
	       'alts
	       (list (list
		      (page-item "Bound" (alg-code "x") ":" (alg-code "<0, 0>"))
		      (page-item "Bound" (alg-code "y") ":" (alg-code "<1, 0>"))
		      (page-item "Bound" (alg-code "z") ":" (alg-code "<0, 1>")))
		     (list
		      (page-para "In general:")
		      (proc-contour '("id_1" "..." "id_n")
				    (alg-code "expr")))))
	 (list (let-contour '("x") (list (alg-code "5")) (alg-code "x"))
	       (blank)
	       'alts
	       (list (list (page-para "In general:")
			   (let-contour '("id_1" "..." "id_n")
					(list (alg-code "expr_1")
					      (alg-code "...")
					      (alg-code "expr_n"))
					(alg-code "expr")))
		     (list
		      (page-item "Bound" (alg-code "x") ":" (alg-code "<0, 0>")))))
	 (list 'alts
	       (let ([mk (lambda (@x)
			   (let-contour (list @x "y") (list (alg-code "5")
							    (alg-code "7"))
					(let-contour '("x") 
						     (list (alg-code @x))
						     (alg-code "+(x, y)"))))])
		 (list (list (mk "x"))
		       (list (mk "@x"))))
	       'next
	       (page-item "Bound" (alg-code "@x") ":" (alg-code "<0, 0>"))
	       'next
	       (page-item "Bound" (alg-code "x") ":" (alg-code "<0, 0>"))
	       'next
	       (page-item "Bound" (alg-code "y") ":" (alg-code "<1, 1>")))
	 (list (letrec-contour '("f" "g")
			       (list (proc-contour '("x") (alg-code "+(x, (g 7))"))
				     (proc-contour '("z") (alg-code "-(z, 2)")))
			       (alg-code "(f 10)"))
	       (blank)
	       'alts
	       (list (list (page-para "In general:")
			   (letrec-contour '("id_1" "..." "id_n")
					   (list (alg-code "expr_1")
						 (alg-code "...")
						 (alg-code "expr_n"))
					   (alg-code "expr")))
		     (list
		      (page-item "Bound" (alg-code "x") ":" (alg-code "<0, 0>"))
		      'next
		      (page-item "Bound" (alg-code "g") ":" (alg-code "<1, 1>"))
		      'next
		      (page-item "Bound" (alg-code "z") ":" (alg-code "<0, 0>"))
		      'next
		      (page-item "Bound" (alg-code "f") ":" (alg-code "<0, 0>"))))))) 

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Lexical Addresses are Static"
   (page-item "The contour approach to computing lexical addresses"
	      "works because they are" (dt "static"))
   (page-item "That's why we can pre-compute them in a compiler"))

  (slide/title
   "Source Language for Compilation"
   'alts
   (list (list
	  (grammar-table
	   (list
	    (alg-code "expr") eqls (alg-code "num") (blank)
	    (blank)           -or- (alg-code "id") (blank)
	    (blank)           -or- (alg-code "prim ( {{ expr }}**, )") (blank)
	    (blank)           -or- (alg-code "let {{ id = expr }}** in expr") (blank)
	    (blank)           -or- (alg-code "proc ( {{ id }}**, ) expr") (blank)
	    (blank)           -or- (alg-code "(expr expr**)") (blank)))
	  (blank)
	  (page-para* "concrete"))
	 (list
	  (grammar-table
	   (list
	    (alg-code "expr") eqls (alg-code "(lit-exp num)") (blank)
	    (blank)           -or- (alg-code "(var-exp symbol)") (blank)
	    (blank)           -or- (alg-code "(primapp-exp prim (list expr**))") (blank)
	    (blank)           -or- (alg-code "(let-exp (list symbol**) (list expr**) expr)") (blank)
	    (blank)           -or- (alg-code "(proc-exp (list symbol**) expr)") (blank)
	    (blank)           -or- (alg-code "(app-exp expr (list expr**))") (blank)))
	  (blank)
	  (page-para* "abstract"))))

  (slide/title
   "Target Language for Compilation"
   (grammar-table
    (list
     (alg-code "cexpr") eqls (alg-code "(lit-cexp num)") (blank)
     (blank)            -or- (alg-code "(var-cexp num num)") (blank)
     (blank)            -or- (alg-code "(primapp-cexp prim (list cexpr**))") (blank)
     (blank)            -or- (alg-code "(let-cexp (list cexpr**) cexpr)") (blank)
     (blank)            -or- (alg-code "(proc-cexp cexpr)") (blank)
     (blank)            -or- (alg-code "(app-cexp cexpr (list cexpr**))") (blank)))
   (blank)
   (page-para* "abstract")
   (page-para* "(no use for concrete)")
   'next
   (page-para "For implementation: declare a" (prog "cexpression")
	      "datatype with" (prog "define-datatype")))

  (slide/title
   "Compilation Function"
   (blank)
   (prog "compile-expression : expr -> cexpr")
   'next
   (blank)
   (page-item "Mostly trival: create a" (alg-code "cexpr")
	      "corresponding to the input" (alg-code "expr"))
   (page-item "Interesting case: " (alg-code "var-exp"))
   'next
   (page-subitem "Use an environment, almost like evaluation")
   'next
   (page-subitem "Key difference #1: instead of"
		 (alg-code "apply-env") ", we need"
		 (alg-code "lexical-address-in-env"))
   'next
   (page-subitem "Key difference #2: no closures; instead, compile a"
		 (alg-code "proc") "body immediately when we encounter"
		 "the" (alg-code "proc")))

  (slide/title
   "Evaluation Function for the Target Language"
   (blank)
   (page-item (prog "eval-cexpression") "is similar to"
	      (prog "eval-expression") ", except:")
   (page-subitem "The names in the environment do not matter")
   (page-subitem "Use" (alg-code "apply-env-to-lexical-address")
		 "instead of" (alg-code "apply-env")))

  (slide/title
   "Implementation"
   (page-para* "(implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
