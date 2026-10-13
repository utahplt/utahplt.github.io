
(module lecture4 (lib "slideshow-run.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let ([ml (lambda i
	      (apply vl-append 
		     line-sep 
		     (map alg-code i)))]
	[cmp (lambda (a b same why)
	       (list (ht-append
		      (* font-size 3)
		      a b)
		     'next
		     (blank)
		     (colorize (bt same) BlueColor)
		     (page-para* why)))])
    (slide/title
     "The Arbitrariness of Variable Names"
     (page-item "Are the following two programs equavalent?")
     (blank)
     'alts
     (list (cmp
	    (ml "(define (f x) (+ x 1))" "(f 10)")
	    (ml "(define (f @y) (+ @y 1))" "(f 10)")
	    "yes" "argument is consistently renamed")
	   (cmp
	    (ml "(define (f x) (+ x 1))" "(f 10)")
	    (ml "(define (f x) (+ @y 1))" "(f 10)")
	    "no" "not a use of the argument anymore")
	   (cmp
	    (ml "(define (f x) (+ x 1))" "(f 10)")
	    (ml "(define (f @y) (+ x 1))" "(f 10)")
	    "no" "not a use of the argument anymore")
	   (cmp
	    (ml "(define (f x) (+ y 1))" "(f 10)")
	    (ml "(define (f @z) (+ y 1))" "(f 10)")
	    "yes" "argument never used, so almost any name is ok")
	   (cmp
	    (ml "(define (f x) (+ y 1))" "(f 10)")
	    (ml "(define (f @y) (+ y 1))" "(f 10)")
	    "no" "now a use of the argument")
	   (cmp
	    (ml "(define (f x) (+ y 1))" "(f 10)")
	    (ml "(define (f x) (+ @z 1))" "(f 10)")
	    "no" "still an undefined variable, but a different one")
	   (cmp
	    (ml "(define (f x)"
		"   (let ([y 10])"
		"      (+ x y)))"
		"(f 0)")
	    (ml "(define (f @z)"
		"   (let ([y 10])"
		"      (+ @z y)))"
		"(f 0)")
	    "yes" "argument is consistently renamed")
	   (cmp
	    (ml "(define (f x)"
		"   (let ([y 10])"
		"      (+ x y)))"
		"(f 0)")
	    (ml "(define (f x)"
		"   (let ([@z 10])"
		"      (+ x @z)))"
		"(f 0)")
	    "yes" "local variable is consistently renamed")
	   (cmp
	    (ml "(define (f x)"
		"   (let ([y 10])"
		"      (+ x y)))"
		"(f 0)")
	    (ml "(define (f x)"
		"   (let ([@x 10])"
		"      (+ x @x)))"
		"(f 0)")
	    "no" "local variable now hides the argument")
	   (cmp
	    (ml "(define (f x)"
		"   (let ([y 10])"
		"      (+ x y)))"
		"(f 0)")
	    (ml "(define (f @y)"
		"   (let ([y 10])"
		"      (+ @y y)))"
		"(f 0)")
	    "no" "local variable now hides the argument"))))

  (slide/title
   "Free and Bound Variables"
   'alts
   (list (list
	  (page-item "A variable for the argument of a function or the"
		     "name of a local variable is a" (dt "binding") (dt "occurrence"))
	  (alg-code "(define (f @x @y) (+ x y z))")
	  (alg-code "(let ([@a 3][@c 4]) (+ a b c))"))
	 (list
	  (page-item "A use of a function argument or a local variable"
		     "is a" (dt "bound") (dt "occurrence"))
	  (alg-code "(define (f x y) (+ @x @y z))")
	  (alg-code "(let ([a 3][c 4]) (+ @a b @c))"))
	 (list
	  (page-item "A use of a variable that is not function argument or a local variable"
		     "is a" (dt "free variable"))
	  (alg-code "(define (f x y) (+ x y @z))")
	  (alg-code "(let ([a 3][c 4]) (+ a @b c))"))))

  (define (let-reduction)
    (vc-append
     (/ font-size 2)
     (let ([top (alg-code "...")])
       (vc-append
	(/ font-size 2)
	(alg-code "... (let ([id_1 val_1]....[id_k val_k]) expr_a) ...")
	(alg-code "->")
	(alg-code "... expr_b ...")
	(page-para* "where" (alg-code "expr_b") "is" (alg-code "expr_a") 
		    "with" (background YellowColor (bt "free")) (alg-code "id_i") "replaced by" (alg-code "val_i"))))))

  (slide/title
   "Evaluating Let"
   (let-reduction)
   'next
   (blank)
   'alts
   (list (list
	  (vl-append
	   line-sep
	   (alg-code "(let ([x 10]) (let ([x 2]) x))")
	   (alg-code "->")
	   (alg-code "(let ([x 2]) x)")
	   (alg-code "->")
	   (alg-code "2")))
	 (list
	  (alg-code* "(let ([x 10])"
		      "   (let ([x (+ @x 1)]) x))"))
	 (list
	  (vl-append
	   line-sep
	   (alg-code* "(let ([x 10])"
		      "   (let ([x (+ x 1)]) x))")
	   (alg-code "->")
	   (alg-code "(let ([x (+ 10 1)]) x)")
	   (alg-code "->")
	   (alg-code "(let ([x 11]) x)  ->  11")))))

  (slide/title
   "Evaluating Function Calls, Revised"
   (beta*-reduction #t #t))

  (slide/title
   "Local Functions"
   (page-para "Recall that")
   (alg-code "(define id_0 (@lambda (id_1....id_k) expr))")
   (page-para "is shorthand for")
   (alg-code "(define (id_0 id_1....id_k) expr)")
   'next
   (page-para (background YellowColor (t "New rule:"))
	      (alg-code "lambda") "is allowed in" (alg-code "let")
	      "bindings to define local functions:")
   (alg-code* "(let ([f (@lambda (x) (+ x 1))])"
	      "   (f 10))"))

  (slide/title
   "Evaluation of Local Functions"
   (vl-append
    line-sep
    (alg-code* "(let ([f (lambda (x) (+ x 1))])"
	       "   (f 10))")
    (alg-code "->")
    (alg-code* "(define f_1073 (lambda (x) (+ x 1)))"
	       "(f_1073 10)")
    (alg-code "->")
    (alg-code* "(define f_1073 (lambda (x) (+ x 1)))"
	       "(+ 10 1)")
    (alg-code "->")
    (alg-code "11")))

  (define (let-fun-reduction)
    (vc-append
     (/ font-size 2)
     (vc-append
      (/ font-size 2)
      (vl-append
       line-sep
       (alg-code "...")
       (alg-code "... (let ([id (lambda (id_1....id_k) expr)]) expr_a) ...")
       (alg-code "->")
       (vl-append
	line-sep
	(alg-code "... (define (id_x id_1....id_k) expr)")
	(alg-code "... expr_b ...")))
      (page-para* "where" (alg-code "expr_b") "is" (alg-code "expr_a") 
		  "with free" (alg-code "id") "replaced by" (alg-code "id_x")
		  "and" (alg-code "_x") "is a subscript that has never been used before,"
		  "and never will be used again"))))

  (slide/title
   "Evaluation of Local Functions"
   (let-fun-reduction))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Lexical Scope"
   (alg-code*
    "(define (f x)"
    "    (let ([g (lambda (y) (+ y @x))])"
    "        (let ([x 2])"
    "            (g 3))))"
    "(f 7)")
   'next
   (blank)
   (page-para* "Will" (alg-code "@x") "be" (alg-code "7") "or" (alg-code "2") "?")
   'next
   (page-para (alg-code "7") ", due to" (dt "lexical scope") ": the value"
	      "of a bound occurrence comes from its binding")
   'next
   (page-para "Need a complete definition of" (dt "free") "and" (dt "bound") "..."))

  (slide/title
   "Free and Bound Variables in Scheme"
   (page-para "For simplicity, we consider a variant of Scheme that is more restricted than usual:")
   (grammar-table
    (list (alg-code "expr") eqls (alg-code "num") (blank)
	  (blank)           -or- (alg-code "id") (blank)
	  (blank)           -or- (alg-code "(+ expr expr)") (blank)
	  (blank)           -or- (alg-code "(let ([id expr]) expr)") (blank)
	  (blank)           -or- (alg-code "(let ([id (lambda (id) expr)]) expr)") (blank)
	  (blank)           -or- (alg-code "(id expr)") (blank))))

  (slide/title/tall
   "Free Variables in Scheme"

   (vl-append
    (* font-size 2/3)
    (page-item (alg-code "num") "has no free variables")
    (page-item (alg-code "id") "has one free variable:" (alg-code "id"))
    (page-item (alg-code "(+ expr_1 expr_2)")
	       "has all the free variables of" (alg-code "expr_1") "and"
	       (alg-code "expr_2") "combined")
    (page-item (alg-code "(let ([id_a expr_b]) expr_a)")
	       "has all the free variables of" (alg-code "expr_a") "minus"
	       (alg-code "id_a") ", plus all the free variables of" (alg-code "expr_b"))
    (page-item (alg-code "(let ([id_a (lambda (id_b) expr_b)]) expr_a)")
	       "has all the free variables of" (alg-code "expr_a") "minus"
	       (alg-code "id_a") ", plus all the free variables of" (alg-code "expr_b") "minus"
	       (alg-code "id_b"))
    (page-item (alg-code "(id expr)")
	       "has all the free variable" (alg-code "id") "plus all the free variables of" (alg-code "expr"))))

  (slide/title
   "Free Variables in Scheme"
   (page-para* "See implementation in Scheme")
   'next
   (blank)
   (page-para* "Reviews" (tt "define-datatype") "motivation and use"))

  (slide/title/tall
   "Bound Variables in Scheme"

   (vl-append
    (* font-size 2/3)
    (page-item (alg-code "num") "has no bound variables")
    (page-item (alg-code "id") "has no bound variables")
    (page-item (alg-code "(+ expr_1 expr_2)")
	       "has all the bound variables of" (alg-code "expr_1") "and"
	       (alg-code "expr_2") "combined")
    (page-item (alg-code "(let ([id_a expr_b]) expr_a)")
	       "has the bound variable" (alg-code "id_a") "if it is free in" (alg-code "expr_a")
	       ", plus the bound variables of" (alg-code "expr_a") "and" (alg-code "expr_b"))
    (page-item (alg-code "(let ([id_a (lambda (id_b) expr_b)]) expr_a)")
	       "has the bound variable" (alg-code "id_a") "if it is free in" (alg-code "expr_a")
	       ", plus the bound variable" (alg-code "id_b") "if it is free in" (alg-code "expr_b")
	       ", plus the bound variables of" (alg-code "expr_a") "and" (alg-code "expr_b") )
    (page-item (alg-code "(id expr)")
	       "has all the bound variables of" (alg-code "expr"))))

  (slide/title
   "let*"
   (page-para (alg-code "let*") "is a shorthand for nested" (hbl-append (alg-code "let") (t "s")))
   (alg-code "(let* ([id_1 expr_1]....[id_k expr_k]) expr)")
   (alg-code "=")
   (alg-code "(let ([id_1 expr_1]) .... (let ([id_k expr_k]) expr)....)")
   'next
   (blank)
   (alg-code "(let ([x 1][y x][z y]) z)  ->> undefined variable x")
   (alg-code "(let* ([x 1][y x][z y]) z)  ->> 1"))

  (slide/title
   "letrec"
   (page-para (alg-code "letrec") "binds its identifiers in local function bodies, as"
	      "well as the main body")
   (vc-append
    (/ font-size 2)
    (vc-append
     (/ font-size 2)
     (vl-append
      line-sep
      (alg-code "...")
      (alg-code "... (letrec ([id (lambda (id_1....id_k) expr_c)]) expr_a) ...")
      (alg-code "->")
      (vl-append
       line-sep
       (alg-code "... (define (id_x id_1....id_k) expr_d)")
       (alg-code "... expr_b ...")))
     (page-para* "where" (alg-code "expr_b") "is" (alg-code "expr_a") 
		 "with free" (alg-code "id") "replaced by" (alg-code "id_x")
		 "," (alg-code "expr_d") "is" (alg-code "expr_c") 
		 "with free" (alg-code "id") "replaced by" (alg-code "id_x")
		 "and" (alg-code "_x") "is a subscript that has never been used before,"
		 "and never will be used again"))))

  (slide/title
   "Free Variables with letrec"

   (page-item (alg-code "(letrec ([id_a (lambda (id_b) expr_b)]) expr_a)")
	      "has all the free variables of" (alg-code "expr_a") "minus"
	      (alg-code "id_a") ", plus all the free variables of" (alg-code "expr_b") "minus"
	      (alg-code "id_a") (background YellowColor (t "and")) (alg-code "id_b")))

  (slide/title
   "Bound Variables with letrec"

   (page-item (alg-code "(let ([id_a (lambda (id_b) expr_b)]) expr_a)")
	      "has the bound variable" (alg-code "id_a") "if it is free in" (alg-code "expr_a") 
	       (background YellowColor (t "or")) (alg-code "expr_b")
	      ", plus the bound variable" (alg-code "id_b") "if it is free in" (alg-code "expr_b")
	      ", plus all the bound variables of" (alg-code "expr_a") "and" (alg-code "expr_b")))
  
  'done)
