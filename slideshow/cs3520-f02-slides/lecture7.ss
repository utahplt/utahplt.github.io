
(module lecture7 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title/center
   "Opening Thought"
   (page-para* "Why must functions always have a name?"))

  (slide/title
   "Anonymous Functions"
   (colorize (page-para "From now on, functions can be anonymous") BlueColor)
   (page-item "Old code")
   (alg-code*
    "(define (eval-rands rands #env# fenv)"
    "  (let ([eval-one (lambda (rand)"
    "                               (#eval-expression# rand #env# fenv))])"
    "    (map eval-one rands)))")
   (page-item "New code")
   (alg-code*
    "(define (eval-rands rands #env# fenv)"
    "  (map (lambda (rand) (#eval-expression# rand #env# fenv))"
    "       rands))"))

  (slide/title
   "Lambda as an Expression"
   (page-para "To suport anonymous functions, we must first")
   (page-item "allow" (alg-code "(lambda (id**) expr)") "as an expression")
   (page-item "change the application grammar to" (alg-code "(expr expr**)"))
   'next
   (grammar-table
    (list (alg-code "expr") eqls (alg-code "num") (blank)
	  (blank)           -or- (alg-code "id") (blank)
	  (blank)           -or- (alg-code "(+ expr expr)") (blank)
	  (blank)           -or- (alg-code "(let ([id expr]**) expr)") (blank)
	  (blank)           -or- (alg-code "(expr expr**)") (blank)
	  (blank)           -or- (alg-code "(lambda (id**) expr)") (blank)))
   'next
   (grammar-table
    (list (alg-code "val")  eqls (alg-code "num") (blank)
	  (blank)           -or- (alg-code "(lambda (id**) expr)") (blank))))

  (slide/title
   "Evaluation with Lambda Expressions"
   (colorize (page-para "Now we need only one kind of" (alg-code "let") "form") BlueColor)
   (page-para
    (alg-code* "(let ([identity (lambda (x) x)])"
	       "   (identity 5))"))
   (page-para 
    (alg-code "->"))
   'next
   (page-para
    (hbl-append (* 3 font-size)
		(alg-code "((lambda (x) x) 5)")
		(colorize (it "usual substitution with values") BlueColor)))
   'next
   (page-para
    (alg-code "->"))
   (page-para
    (hbl-append (* 3 font-size)
		(alg-code "5")
		(colorize (it "new procedure application rule...") BlueColor))))

  (slide/title
   "New Application Rule"
   (vc-append
    (/ font-size 2)
    (alg-code "... ((lambda (id_1....id_k) expr_a) val_1....val_k) ...")
    (alg-code "->")
    (alg-code "... expr_b ..."))
   (page-para* "where" (alg-code "expr_b") "is" (alg-code "expr_a") 
	       "with free" (alg-code "id_i") 
	       "replaced by" (alg-code "val_i"))
   'next
   (blank)
   (blank)
   'alts 
   (list (list (alg-code "((lambda (x) x) 5) -> 5"))
	 (list (alg-code "((lambda (x y) (+ x y)) 2 3) -> (+ 2 3) -> 5"))))

  (define (feed-ex red?)
    (slide/title
     "Using Anonymous Functions"
     (page-para "Using anonymous functions, we can easily feed a list of fish:")
     (vl-append (alg-code ";; feed all fish 1 lb of food:")
		(alg-code$ (and red? ".lambda .x. .. x ...")
			   "(map (lambda (x) (+ x 1)) '(4 5 8))")
		(alg-code "  = '(5 6 9)"))
     (if red? 'nothing 'next)
     (vl-append (alg-code ";; feed all fish 2 lbs of food:")
		(alg-code$ (and red? ".lambda .x. .. x ...")
			   "(map (lambda (x) (+ x 2)) '(5 6 9))")
		(alg-code "  = '(7 8 11)"))
     (if red?
	 (colorize (page-para* "Avoid cut-and-paste of the" (alg-code "lambda") "expression?") RedColor)
	 'nothing)))
  (feed-ex #f)
  (feed-ex #t)

  (slide/title
   "Functions that Return Functions"
   (alg-code* ";; make-feeder : num -> (num -> num)"
	      "(define (make-feeder amt)"
	      "  (lambda (x) (+ x amt)))"
	      " "
	      ";; feed all fish 1 lb of food:"
	      "(map (make-feeder 1) '(4 5 8))"
	      "  = '(5 6 9)"
	      " "
	      ";; feed all fish 2 lbs of food:"
	      "(map (make-feeder 2) '(5 6 9))"
	      "  = '(7 8 11)"))

  (slide/title
   "Another Example with Procedures as Values"
   (alg-code* "(let ([mk-add (lambda (x) (lambda (y) (+ x y)))])"
	      "   (let ([add5 (mk-add 5)])"
	      "      (add5 7)))"
	      "->"
	      "(let ([add5 ((lambda (x) (lambda (y) (+ x y))) 5)])"
	      "   (add5 7))"
	      "->"
	      "(let ([add5 (lambda (y) (+ 5 y))])"
	      "   (add5 7))"
	      "->"
	      "((lambda (y) (+ 5 y)) 7)"
	      "->"
	      "(+ 5 7)  ->  12"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Teminology: First-Order and Higher-Order"
   'alts
   (list
    (list
     (page-item "The procedures supported by top-level definitions are"
		(dt "first-order") "procedures")
     (page-subitem "A procedure cannot consume or produce a procedure")
     (page-subitem "Methods in Java and procedures in Fortran are first-order")
     (page-subitem "Functions C are first-order, but function pointers are values"))
    (list
     (page-item "The procedures supported by" (alg-code "lambda") "are"
		(dt "higher-order") "procedures")
     (page-subitem "A procedure can return a procedure that returns a"
		   "procedure that consumes a procedure that returns a procedure...")
     (page-subitem "Procedures in Scheme are higher-order"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title 
   "Procedure Expressions in the Book Language"
   (page-para "Concrete extensions:")
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "expr") (blank)
     (alg-code "expr")    eqls (alg-code "proc (id**,) expr") (blank)
     (blank)              -or- (alg-code "(expr expr**)") (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code* "let identity = proc(x) x"
			  " in (identity 5)"
			  "->> 5"))
	 (list (alg-code* "let sum = proc(x, y, z) +(x, +(y, z))"
			  " in (sum 10 20 30)"
			  "->> 60"))
	 (list (alg-code* "(proc(x) x 5)"
			  "->> 5"))
	 (list (alg-code* "let mkadd = proc(x) proc(y) +(x, y)"
			  " in let add5 = (mkadd 5)"
			  "     in let x = 10"
			  "         in (add5 6)")
	       'next
	       (alg-code "->> 11"))))

  (define (build-view l)
    (define head (list (it "Expr") (it "Env")))
    (define arr (arrow (/ font-size 2) 0))
    (define (tab nt)
      (scale
       (table 2 nt lc-superimpose lc-superimpose (* 1 font-size) font-size)
       0.8))
    (let loop ([l l][t head][indent (blank)][dup-last? #f])
      (if (null? l)
	  null
	  (let* ([mk-nt
		  (lambda (indent) 
		    (append t
			    (list (hbl-append indent (caar l))
				  (cadar l))))]
		 [nt (mk-nt (colorize indent RedColor))]
		 [keep? (equal? (cdddar l) '(*))]
		 [msgs (caddar l)])
	    (cons
	     (list* (tab nt)
		    (blank)
		    (or (and msgs (car msgs))
			null))
	     (append
	      (if keep?
		  null
		  (loop (cdddar l) nt (if (zero? (pict-width indent))
					  arr
					  (inset indent font-size 0 0 0))
			#t))
	      (loop (cdr l) (if keep? (mk-nt (colorize indent GreenColor)) t) indent dup-last?)
	      (if (and dup-last? (null? (cdr l)))
		  (list (list* (tab (mk-nt (colorize indent GreenColor)))
			       (blank)
			       (or (and msgs
					(cadr msgs))
				   null)))
		  null)))))))
	      

  (slide/title 
   "Evaluation with Environments"
   'alts
   (build-view
    (list (list (alg-code* "let x = +(2, 3)"
			   "  in x")
		(alg-code "{ }")
		(list (list (page-item "This trace shows the expression and environment arguments"
				       "to" (tt "eval-expresson")))
		      #f)

		(list (alg-code "+(2, 3)")
		      (alg-code "{ }")
		      (list (list (page-item "Arrows show nested recursive calls"))
			    #f))
		
		(list (alg-code "5")
		      (alg-code "{ }")
		      (list #f
			    (list (page-item "Eventually a value is reached for each recursive call")
				  (page-item "To continue with"
					     (alg-code "let") ", extend the environment and"
					     "evaluate the body")))))
	  
	  (list (alg-code "x")
		(alg-code "{ x = 5 }")
		(list (list (page-item "Drop the context for the recursive body evaluation, since"
				       "it isn't needed"))
		      #f))

	  (list (alg-code "5")
		(alg-code "{ x = 5 }")
		#f))))

  (slide/title 
   "Evaluation with Environments"
   'alts
   (build-view
    (list (list (alg-code* "let x = 5"
			   "  in let x = 6"
			   "     in x")
		(alg-code "{ }")
		(list (list (page-item "Another example: nested" (alg-code "let")))
		      #f)
		
		(list (alg-code "5")
		      (alg-code "{ }")
		      #f))

	  (list (alg-code* "let x = 6"
			   "  in x")
		(alg-code "{ x = 5 }")
		#f

		(list (alg-code "6")
		      (alg-code "{ x = 5 }")
		      
		      (list #f
			    (list (page-item "New value for" (alg-code "x")
					     "replaces the old one for the body")))))

	  (list (alg-code "x")
		(alg-code "{ x = 6 }")
		#f)

	  (list (alg-code "6")
		(alg-code "{ x = 6 }")
		#f))))

  (slide/title 
   "Evaluation with Environments"
   'alts
   (build-view
    (list (list (alg-code* "let x = 5"
			   "  in let y = let x = 6 in x"
			   "     in x")
		(alg-code "{ }")
		(list (list (page-item "Another example:" (alg-code "let") "nested in a different way"))
		      #f)

		(list (alg-code "5")
		      (alg-code "{ }")
		      #f))

	  (list (alg-code* "let y = let x = 6 in x"
			   "  in x")
		(alg-code "{ x = 5 }")
		#f

		(list (alg-code "let x = 6 in x")
		      (alg-code "{ x = 5 }")
		      #f

		      (list (alg-code "6")
			    (alg-code "{ x = 5 }")
			    #f))
		
		(list (alg-code "x")
		      (alg-code "{ x = 6 }")
		      #f)
		
		(list (alg-code "6")
		      (alg-code "{ x = 6 }")
		      (list #f
			    (list 
			     'alts
			     (list (list (page-item "What environment is extended with"
						    (alg-code "y = 6")
						    "?"))
				   (list (page-item "Answer: the original one for the"
						    (alg-code "let") "of" (alg-code "y"))))))))
	  
	  (list (alg-code "x")
		(alg-code "{ x = 5, y = 6 }")
		#f)

	  (list (alg-code "5")
		(alg-code "{ x = 5, y = 6 }")
		#f))))

  (define mkadd-env (alg-code* "{ mkadd = proc(x) proc(y) +(x, y) }"))
  (define big-env (alg-code* "{ mkadd = proc(x) proc(y) +(x, y)"
			     "   add5 = proc (y) +(x, y) }"))

  (slide/title 
   "Evaluation with Procedures and Environments"
   'alts
   (build-view
    (list (list (alg-code* "let mkadd = proc(x) proc(y) +(x, y)"
			   " in let add5 = (mkadd 5)"
			   "     in (add5 6)")
		(alg-code* "{ }")
		#f

		(list (alg-code "proc(x) proc(y) +(x, y)")
		      (alg-code* "{}")
		      (list (list (page-item "Is a" (alg-code "proc") "expression a value?")
				  'next
				  (page-item "A" (alg-code "lambda") "was a value in Scheme... so let's say it's ok")
				  'next
				  (blank)
				  (background
				   YellowColor
				   (page-para* "this choice will turn out to be slightly wrong")))
			    #f)))
	  (list (alg-code* "let add5 = (mkadd 5)"
			   "  in (add5 6)")
		mkadd-env
		#f
		
		(list (alg-code "(mkadd 5)")
		      mkadd-env
		      #f
		      
		      (list (alg-code "mkadd")
			    mkadd-env
			    #f)
		      
		      (list (alg-code "proc(x) proc(y) +(x, y)")
			    mkadd-env
			    #f

			    '*)
		      
		      (list (alg-code "5")
			    mkadd-env
			    (list #f
				  (list (page-item "To evaluate an application, extend the application's"
						   "environment with a binding for the argument")
					'next
					(blank)
					(background
					 YellowColor
					 (page-para* "this isn't quite right, either"))))))

		(list (alg-code "proc (y) +(x, y)")
		      (alg-code* "{ mkadd = proc(x) proc(y) +(x, y)"
				 "   x = 5 }")
		      (list #f
			    (list (page-item "So the value for" (alg-code "add5")
					     "is also a procedure")
				  (page-item "Extend the original environment"
					     "for the" (alg-code "let"))))))
	  
	  (list (alg-code* "(add5 6)")
		(alg-code* "{ mkadd = proc(x) proc(y) +(x, y)"
			   "   add5 = proc (y) +(x, y) }")
		(list (list 'alts
			    (list (list (page-item "We can see where this is going..."
						   (alg-code "x") "has no value")
					(page-item "What went wrong?"))
				  (list (page-item "In Scheme, procedures as values worked because they"
						   "had eager substitutions"))
				  (list (page-item "With lazy substitutions: combine a"
						   (alg-code "proc") "and an environment to get a value")
					(page-item "The combination is called a" (dt "closure")))))
		      #f)))))


  (define mkadd-env2 (alg-code* "{ mkadd = <(x), proc(y) +(x, y), { }> }"))
  (define big-env2 (alg-code* "{ mkadd = <(x), proc(y) +(x, y), { }>"
			      "   add5 = <(y), +(x, y), { x = 5 }> }"))

  (slide/title 
   "Evaluation with Closures"
   'alts
   (build-view
    (list (list (alg-code* "let mkadd = proc(x) proc(y) +(x, y)"
			   " in let add5 = (mkadd 5)"
			   "     in (add5 6)")
		(alg-code* "{ }")
		#f

		(list (alg-code "proc(x) proc(y) +(x, y)")
		      (alg-code* "{}")
		      #f)

		(list (alg-code "<proc(x) proc(y) +(x, y), { }>")
		      (alg-code* "{}")
		      (list (list (page-item "Create a closure with the current environment to get a value"))
			    #f))
		
		(list (alg-code "<(x), proc(y) +(x, y), { }>")
		      (alg-code* "{}")
		      (list (list (page-item "Alternate form: arguments, body, and environment"))
			    (list (page-item "A closure is a value")))))

	  (list (alg-code* "let add5 = (mkadd 5)"
			   "  in (add5 6)")
		mkadd-env2
		#f
		
		(list (alg-code "(mkadd 5)")
		      mkadd-env2
		      #f
		      
		      (list (alg-code "mkadd")
			    mkadd-env2
			    #f)
		      
		      (list (alg-code "<(x), proc(y) +(x, y), { }>")
			    mkadd-env2
			    #f

			    '*)
		      
		      (list (alg-code "5")
			    mkadd-env2
			    (list #f
				  (list (page-item "To evaluate an application, extend the"
						   (it "closure's")
						   "environment with a binding for the argument")))))

		(list (alg-code "proc (y) +(x, y)")
		      (alg-code* "{ x = 5 }")
		      #f)

		(list (alg-code* "<(y), +(x, y), { x = 5 }>")
		      (alg-code* "{ x = 5 }")
		      (list (list (page-item "Again, create a closure")
				  (page-item "Note that the" (alg-code "x")
					     "binding is saved in the closure"))
			    #f)))
		
	  (list (alg-code* "(add5 6)")
		big-env2
		#f

		(list (alg-code "add5")
		      big-env2
		      #f)
		
		(list (alg-code* "<(y), +(x, y), { x = 5 }>")
		      big-env2
		      #f
		
		      '*)

		(list (alg-code "6")
		      big-env2
		      (list #f
			    (list (page-item "Extend the closure's environment"
					     (alg-code "{ x = 5 }")
					     "with a"
					     "binding for" (alg-code "y"))))))

	  (list (alg-code "+(x, y)")
		(alg-code "{ x = 5, y = 6 }")
		(list (list (page-item "This is clearly going to work"))
		      #f)))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define xy-env (env-extension (list "x" "y")
				(list "1" "2")
				#f
				empty-env))

  (define (add-f-closure z-name clos-only?)
    (closure-env clos-only? 0 
		 (env-pict xy-env) (env-frame xy-env)
		 (env-pict xy-env) 
		 "f" z-name (format "+(~a, y)" z-name)
		 #f))

  (define xy-env+f (add-f-closure "z" #t))
  (define xy+f-env (add-f-closure "z" #f))

  (define (add-z z-name base-env val vert?)
    (add-binding font-size vert? z-name val #f (env-pict base-env) (env-frame xy-env)))
  (define xy+f+z-env (add-z "z" xy+f-env "2" #t))

  (define (explain p)
    (rt-superimpose
     p
     (vl-append
      line-sep
      (it "top purple arrow points to")
      (it "the current environment")
      (blank font-size)
      (it "purple in bottom area hilites")
      (it "the current expression"))))
  
  (define (prog1 p)
    (page-para
     (alg-code$* p
		 "let x = 1  y = 2"
		 "  in +(x, y)")))

  (define (prog2-pre z-name body p)
    (page-para
     (alg-code$* p
		 "let x = 1  y = 2"
		 (format "  in let f = proc (~a) +(~a, y)" z-name z-name)
		 (format "    in ~a" body))))

  (define (prog2 p)
    (prog2-pre "z" "(f y)" p))

  (slide/title
   "Environments in Picture Form"
   'alts
   (list (list (explain (show-env empty-env (env-frame empty-env)))
	       (prog1 ".*"))
	 (list (explain (show-env xy-env (env-frame xy-env)))
	       (prog1 "[+].x, y."))
	 (list (show-env empty-env (env-frame empty-env))
	       (prog2 ".*"))
	 (list (show-env xy-env (env-frame xy-env))
	       (prog2 "(let f.*)|    .*"))
	 (list (show-env xy-env (env-frame xy-env))
	       (prog2 "proc.*"))
	 (list (show-env xy-env+f (env-frame xy-env))
	       (prog2 "proc.*"))
	 (list (show-env xy+f-env (env-frame xy+f-env))
	       (prog2 ".f y."))
	 (list (show-env xy+f+z-env (env-frame xy+f+z-env))
	       (prog2 "[+].z, y."))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title 
   "Procedure Expressions in the Book Language"
   (page-para "Abstract extensions:")
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "(a-program expr)") (blank)
     (alg-code "expr")    eqls (alg-code "(proc-exp (list id**) expr)") (blank)
     (blank)              -or- (alg-code "(app-exp expr (list expr**))") (blank)
     (alg-code "val")     eqls (alg-code "num") (blank)
     (blank)              -or- (alg-code "Proc") (blank)
     (alg-code "Proc")     eqls (alg-code "(closure (list id**) expr env)") (blank))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)
