
(module lecture5 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title 
   "Recap: Concrete and Abstract Syntax"
   (page-item "Every language" (it "X") "has one" (dt "concrete syntax"))
   (page-item "Programmers using language" (it "X") "write programs using the concrete syntax")
   'next
   (page-item "To represent programs in language" (it "X") "for processing with language" (hbl-append (it "Y") (t ","))
	      "we represent" (dt "abstract syntax") "for" (it "X") "programs")
   (page-item "The representation is specific to" (it "X") "in" (it "Y") 
	      ", but there is more than one choice")
   'alts
   (list (list (alg-code "'(+ 1 2)")
	       (alg-code "(plus (number 1) (number 2))"))
	 (list))
   (page-item "Abstract syntax is" (dt "abstract") "because it omits irrelevant details")
   (page-para* "(``irrelevant'' depends on the analysis task)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Concrete Syntax for the Book Language"
   (grammar-table
    (list
     (alg-code "prog") eqls (alg-code "expr") (blank)
     (alg-code "expr") eqls (alg-code "num") (blank)
     (blank)           -or- (alg-code "id") (blank)
     (blank)           -or- (alg-code "prim ( {{ expr }}**, )") (blank)
     (alg-code "prim")  eqls (alg-code "+ | - | * | add1 | sub1") (blank)))
   'next
   (blank)
   (page-para "Example:")
   'alts
   (list (list (alg-code "1"))
	 (list (alg-code "x"))
	 (list (alg-code "+(1, 2)"))
	 (list (alg-code "+(1, 2, 3)"))
	 (list (alg-code "add1(1)"))
	 (list (alg-code "add1(+(2, x))"))))

  (slide/title
   "Representation for the Book Language"
   (grammar-table
    (list
     (alg-code "prog") eqls (alg-code "(a-program expr)") (blank)
     (alg-code "expr") eqls (alg-code "(lit-exp num)") (blank)
     (blank)           -or- (alg-code "(var-exp symbol)") (blank)
     (blank)           -or- (alg-code "(primapp-exp prim (list expr**))") (blank)
     (alg-code "prim") eqls (alg-code "(add-prim) | (subtract-prim)") (blank)
     (blank)           -or- (alg-code "(mult-prim) | (inc-prim) | (decr-prim)") (blank)))
   'next
   (blank)
   'alts
   (let ([ca (lambda (c a)
	       (list
		(page-para
		 (hbl-append
		  (* 5 font-size)
		  (t "Concrete:")
		  c))
		(page-para "Abstract representation:")
		a))])
     (list (ca (alg-code "1") (alg-code "(a-program (lit-exp 1))"))
	   (ca (alg-code "x") (alg-code "(a-program (var-exp 'x))"))
	   (ca (alg-code "+(1, 2)") (alg-code* "(a-program"
					       "  (primapp-exp (add-prim) (list (lit-exp 1) (lit-exp 2))))"))
	   (list (page-para* "But the connection between concrete and abstract/representation examples"
			     "is only in our heads right now...")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Parsing"
   (page-item "Converting concrete syntax to abstract syntax is the job of a" (dt "parser"))
   (page-item "Parsing is a deep topic with a long history...")
   'next
   (page-item "... that we will ignore almost entirely")
   'next
   (page-item "The EoPL extensions to Scheme include a parser generator called" (dt "SLLGEN"))
   'next
   (page-para* "(see parser example in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Ways of Evaluating"
   (page-item "So far:")
   (alg-code "*(+(3, 4), -(2,1))  ->  *(7, -(2,1))  ->  *(7,1)  -> 7")
   'next
   (page-item "Alternative:")
   (infer (alg-code "*(+(3,4), -(2,1)) = 7")
	  (ante-append (alg-code "+(3,4) = 7")
		       (alg-code "-(2,1) = 1")))
   'next
   (page-para "In other words, to evaluate an expression, first evaluate the sub-expressions, then combine their values")
   'next
   (colorize (page-para* "=> a recursive" (tt "eval-expression") "function") BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "eval-expression"
   (page-para* "(implementation in DrScheme)")
   'next
   (page-item "Note: evaluating an identifier is an error for now"))
  
  (slide/title
   "Add Conditionals"
   (page-item "Concrete:")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "if expr then expr else expr") (blank)))
   (page-item "Abstract:")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "(if-exp expr expr expr)") (blank)))
   (blank)
   (page-para* "(update implementation in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Add Local Bindings"
   (page-item "Concrete:")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "let {{ id = expr }}** in expr") (blank)))
   (page-item "Abstract:")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "(let-exp (list symbol**) (list expr**) expr)") (blank)))
   'next
   (blank)
   (page-para "Evaluating an identifier isn't an error anymore..."
	      "but how does" (tt "eval-expression") "know the value of the identifier?"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Evaluating Let"
   (page-item "One possibility: for" (alg-code "let-exp") "expressions,"
	      (tt "eval-expression") "could call" (tt "substitute") "on the body")
   'next
   (page-item "Another possibility: " (tt "eval-expression") "can perform the substitution lazily, as it goes")
   'next
   (page-subitem (tt "eval-expression") "now takes two arguments: an expression and a set of lazy substitutions")
   (page-subitem "the set of lazy substitutions is called an" (dt "environment")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Environments"
   (page-para "Implement environments as an ADT with three operations:")
   (page-item (alg-code "(empty-env)") " : creates an empty environment; i.e., no substitutions")
   (page-item (alg-code "(extend-env env (list symbol**) (list val**))") 
	      " : creates a new environment that has the"
	      "substitutions of" (alg-code "env") ", plus (or instead of) the substitution of each" 
	      (alg-code "symbol") "with" (alg-code "val"))
   (page-item (alg-code "(apply-env env symbol)") " : extracts the substitution of" (alg-code "symbol")
	      "from" (alg-code "env")))

  (slide/title
   "Environment Examples"
   (blank)
   'alts
   (list (list
	  (alg-code* "(let ([s (extend-env '(x) '(1) (empty-env))])"
		     "    (apply-env s 'x))"
		     "->> 1"))
	 (list
	  (alg-code* "(let ([s (extend-env '(x y z) '(1 2 3) (empty-env))])"
		     "    (apply-env s 'y)"
		     "->> 2"))
	 (list
	  (alg-code* "(let ([s (extend-env '(x y z) '(1 2 3) (empty-env))])"
		     "    (let ([t (extend-env '(a y) '(5 6) s)])"
		     "        (apply-env t 'a)"
		     "->> 5"))
	 (list
	  (alg-code* "(let ([s (extend-env '(x y z) '(1 2 3) (empty-env))])"
		     "    (let ([t (extend-env '(a y) '(5 6) s)])"
		     "        (apply-env t 'y)"
		     "->> 6"))
	 (list
	  (alg-code* "(apply-env (empty-env) 'x)"
		     "->> error"))))

  (slide/title
   "Implementing Let"
   (page-para* "(update implementation in DrScheme)"))

  'done)
