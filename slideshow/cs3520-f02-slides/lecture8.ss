
(module lecture8 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss"
	   (lib "string.ss"))

  (slide/title
   "Terminology: Denoted and Expressed Values"
   (page-item "A" (dt "denoted value")
	      "is the meaning of a variable")
   (page-item "An" (dt "expressed value")
	      "is the result of an expression")
   (blank)
   (page-para* "The set of denoted and expressed values can be different"))

  (slide/title
   "Terminology: Denoted and Expressed Values"
   (page-item "First-order functions")
   (page-subitem "denoted values: numbers and functions")
   (page-subitem "expressed values: numbers")
   (page-item "Higher-order functions")
   (page-subitem "denoted values: numbers and functions")
   (page-subitem "expressed values: numbers and functons"))

  (slide/title 
   "Procedure Expressions: Concrete Syntax"
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "expr") (blank)
     (alg-code "expr")    eqls (alg-code "proc (id**,) expr") (blank)
     (blank)              -or- (alg-code "(expr expr**)") (blank)))
   (blank)
   (alg-code* "let identity = proc(x) x"
	      " in (identity 5)"))

  (slide/title 
   "Procedure Expressions: Abstract Syntax"
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "(a-program expr)") (blank)
     (alg-code "expr")    eqls (alg-code "(proc-exp (list id**) expr)") (blank)
     (blank)              -or- (alg-code "(app-exp expr (list expr**))") (blank)
     (alg-code "val")     eqls (alg-code "num | Proc") (blank)
     (alg-code "Proc")     eqls (alg-code "(closure (list id**) expr env)") (blank)))
   (blank)
   (alg-code* "(a-program"
	      "  (let-exp (list 'identity)"
	      "           (list (proc-exp (list 'x) (var-exp 'x)))"
	      "           (app-exp (var-exp 'identity) (list-exp 5))))"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Implementing Procedures"
   (page-para* "(implementation in DrScheme)")
   'next
   (blank)
   (page-para "New representation of environments:")
   (prog*
    "(define-datatype environment environment?"
    "  (empty-env-record)"
    "  (extended-env-record"
    "    (syms (list-of symbol?))"
    "    (vals (list-of denval?))"
    "    (env environment?)))"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Recursion"
   (page-para "Suppose we try to write the" (alg-code "fact")
	      "function using only" (alg-code "let"))
   (blank)
   (alg-code* "let fact = proc(n) if n then *(n, (fact -(n, 1))) else 1"
	      " in (fact 10)")
   (blank)
   (page-para "The above doesn't work, because" (alg-code "fact")
	      "is not bound in the local function")
   'next
   (blank)
   (page-para "We'll add" (alg-code "letrec") ", but first we'll see"
	      "how to implement" (alg-code "fact") "without it..."))

  
  (slide/title
   "Recursion with Let"
   (page-item (colorize (t "Problem:") RedColor) (alg-code "fact") "can't see itself")
   'next
   (page-item (colorize (t "Note:") BlueColor) "anyone calling" (alg-code "fact") "can see" (alg-code "fact"))
   'next
   (page-item (colorize (t "Idea:") GreenColor) "have the caller supply" (alg-code "fact") "to" (alg-code "fact") "(along with a number)")
   'next
   (blank)
   (alg-code* "let fact = proc(n, @f) if n then *(n, (@f -(n, 1) @f)) else 1"
	      " in (fact 10 @fact)")
   'next
   (blank)
   (background YellowColor (inset (it "this works!") (/ font-size 4))))

  (slide/title
   "What Happened?"
   (page-item "The key insight is delaying some work to the caller")
   (page-item "We can exploit this idea to implement" (alg-code "letrec")
	      ", but in a slightly different way")
   'next
   (blank)
   (page-item (alg-code "letrec") "requires a" (it "closure") "that refers to itself")
   (page-item "We can delay the actual construction of the closure"
	      "until it is extracted from the environment"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define fact-body "if n then *(n, (fact^-(n, 1))) else 1")

  (define (prog3&4 rec p)
    (page-para
     (alg-code$* p
		 (format "let~a fact = proc(n) ~a" rec fact-body)
		 " in (fact  10)")))

  (define (prog3 p) (prog3&4 "" p))
  (define (prog4 p) (prog3&4 "rec" p))

  (define (mk-fact-env clos-only?)
    (closure-env clos-only? 0 
		 (env-pict empty-env) (env-frame empty-env)
		 (env-pict empty-env) 
		 "fact" "n" fact-body
		 #f))

  (define empty-env+fact (mk-fact-env #t))
  (define fact-env (mk-fact-env #f))

  (define fact+n-env (add-binding 0 #t "n" "10" #f (env-pict fact-env) (env-frame empty-env)))

  (define rec-fact-env (env-extension (list "fact")
				      (list (cons (alg-code "n") (alg-code fact-body)))
				      #f
				      empty-env))

  (define (add-fact-clos base-env)
    (closure-env #t 0 (env-pict base-env) 
		 (env-frame base-env) 
		 (env-pict rec-fact-env)
		 "fact" "n" fact-body #f))
    
  (define rec-fact+fact-env (add-fact-clos rec-fact-env))

  (define (add-n base-env val)
    (add-binding 0 #t "n" val #f 
		 (env-pict base-env) 
		 (env-frame rec-fact-env)))

  (define rec-fact+n-env (add-n rec-fact+fact-env "10"))

  (define rec-fact+fact+fact-env (add-fact-clos rec-fact+n-env))

  (define rec-fact+n2-env (add-n rec-fact+fact+fact-env "9"))

  (slide/title
   "Recursive Environments for Recursive Functions"
   'alts
   (list (list (rc-superimpose
                (show-env empty-env (env-frame empty-env))
                (it "This isn't going to work"))
	       (prog3 ".*"))
	 (list (show-env empty-env (env-frame empty-env))
	       (prog3 "proc.*"))
	 (list (show-env empty-env+fact (env-frame empty-env))
	       (prog3 "proc.*"))
	 (list (show-env fact-env (env-frame fact-env))
	       (prog3 ".fact  10."))
	 (list (show-env fact+n-env (env-frame fact+n-env))
	       (prog3 (regexp-quote fact-body)))
	 (list (rc-superimpose
		(show-env fact+n-env (env-frame fact+n-env))
		(page-para* (it "No binding for") (alg-code "fact")))
	       (prog3 ".[(]n, .fact[^)]*[)][)][)]"))
	 
	 (list (show-env empty-env (env-frame empty-env))
	       (prog4 ".*"))
	 (list (rc-superimpose
		(show-env rec-fact-env (env-frame rec-fact-env))
		(vl-append
		 line-sep
		 (it "double box means a recursively")
		 (it "extended environment")))
	       (prog4 ".fact  10."))
	 (list (show-env rec-fact-env (env-frame rec-fact-env))
	       (prog4 "fact  "))
	 (list (rb-superimpose 
		(show-env rec-fact+fact-env (env-frame rec-fact-env))
		(vl-append
		 line-sep
		 (hbl-append (it "every lookup of ") (alg-code "fact"))
		 (it "generates a closure")
		 (t "")))
	       (prog4 "fact  "))
	 (list (show-env rec-fact+fact-env (env-frame rec-fact-env))
	       (prog4 ".fact  10."))
	 (list (show-env rec-fact+n-env (env-frame rec-fact+n-env))
	       (prog4 (regexp-quote fact-body)))
	 (list (show-env rec-fact+n-env (env-frame rec-fact+n-env))
	       (prog4 "[*].n, .fact.-.n, 1..."))
	 (list (show-env rec-fact+n-env (env-frame rec-fact+n-env))
	       (prog4 ".fact.-.n, 1.."))
	 (list (show-env rec-fact+n-env (env-frame rec-fact+n-env))
	       (prog4 "fact\\^"))
	 (list (show-env rec-fact+fact+fact-env (env-frame rec-fact+n-env))
	       (prog4 "fact\\^"))
	 (list (show-env rec-fact+fact+fact-env (env-frame rec-fact+n-env))
	       (prog4 ".fact.-.n, 1.."))
	 (list (show-env rec-fact+n2-env (env-frame rec-fact+n2-env))
	       (prog4 (regexp-quote fact-body)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Implementing Recursively Extended Envs"
   (prog*
    "(define-datatype environment environment?"
    "  (empty-env-record)"
    "  (extended-env-record"
    "    (syms (list-of symbol?))"
    "    (vals (list-of denval?))"
    "    (env environment?))"
    "  (recursively-extended-env-record"
    "    (proc-names (list-of symbol?))"
    "    (idss (list-of (list-of symbol?)))"
    "    (bodies (list-of expression?))"
    "    (env environment?)))"))

  (slide/title
   "Implementing letrec"
   (page-para* "(implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Back to Recursion with Let..."
   (page-item "Allowing functions to be values is a powerful idea")
   'next
   (page-item "As it turns out, we don't even need" (alg-code "let") "!")
   (blank)
   'alts
   (list (list
	  (alg-code "let id_1 = expr_1 ... id_n = expr_n in expr")
	  (t "is the same as")
	  (alg-code "(proc(id_1, ... id_n) expr  expr_1 ... expr_n)"))
	 (list
	  (alg-code "(let ([id_1 expr_1] ... [id_n = expr_n])  expr)")
	  (t "is the same as")
	  (alg-code "((lambda (id_1 ... id_n) expr) expr_1 ... expr_n)"))))
  
  (slide/title
   "The Lambda Calculus"
   (page-item "We don't even need functions of multiple arguments...")
   (alg-code* "((lambda (id_1 ... id_n) expr)"
              "  expr_1 ... expr_n)")
   (t "is the same as")
   (alg-code* "(((lambda (id_1) ... (lambda (id_n) expr))"
              "    expr_1) ... "
	      "  expr_n)")
   'next
   (page-para "Passing multiple arguments one-at-a-time is called" (dt "currying"))
   'next
   (page-para "The" (dt "lambda calculus") "has only single-argument" (alg-code "lambda")
	      "and single-argument function calls, and it's computationally complete"))


  'done)
