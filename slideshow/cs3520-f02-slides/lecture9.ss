
(module lecture9 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss"
	   (lib "string.ss"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/center
   (page-para* (bt "Reminder"))
   (page-para* "Mid-Term 1 is Tuesday next week")
   (blank)
   (blank)
   (t "(No homework will be assigned this week)"))

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

  (define xy+f-env (add-f-closure "z" #f))

  (define (add-z z-name base-env val vert?)
    (add-binding font-size vert? z-name val #f (env-pict base-env) (env-frame xy-env)))
  (define xy+f+z-env (add-z "z" xy+f-env "2" #t))

  (define (prog2-pre z-name body p)
    (page-para
     (alg-code$* p
		 "let x = 1  y = 2"
		 (format "  in let f = proc (~a) +(~a, y)" z-name z-name)
		 (format "    in ~a" body))))

  (define (prog2 p)
    (prog2-pre "z" "(f y)" p))

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
   "Recap: Recursive Environments"
   'alts
   (list (list (show-env empty-env (env-frame empty-env))
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define rec!-fact-env-pre (env-extension
			     (list "fact")
			     (list "0")
			     #f
			     empty-env))

  (define (mk-rec!-fact-env clos-only?)
    (closure-env clos-only? 0 
		 (env-pict empty-env) (env-frame empty-env)
		 'self
		 "fact" "n" fact-body
		 #f))

  (define rec!-fact-env-pre+ (make-env
			      (lt-superimpose
			       (env-pict rec!-fact-env-pre)
			       (env-pict (mk-rec!-fact-env #t)))
			      (env-frame rec!-fact-env-pre)
			      null))

  (define rec!-fact-env (mk-rec!-fact-env #f))

  (define (add-fact-n base-env val)
    (add-binding 0 #t "n" val #f 
		 (env-pict base-env) 
		 (env-frame rec!-fact-env)))
    
  (define rec!-fact+n-env (add-fact-n rec!-fact-env "10"))
  (define rec!-fact+n+n-env (add-fact-n rec!-fact+n-env "9"))

  (slide/title
   "Another Approach to Recursive Closures"
   'alts
   (list (list (rc-superimpose
		(show-env empty-env (env-frame empty-env))
		(vl-append
		 line-sep
		 (it "alternate approach...")))
	       (prog4 ".*"))
	 (list (rc-superimpose
		(show-env rec!-fact-env-pre (env-frame empty-env))
		(vl-append
		 line-sep
		 (it "create an environment")
		 (it "with a dummy value...")))
	       (prog4 ".*"))
	 (list (rc-superimpose
		(show-env rec!-fact-env-pre+ (env-frame empty-env))
		(vl-append
		 line-sep
		 (it "create the closure using")
		 (it "the environment...")))
	       (prog4 ".*"))
	 (list (rc-superimpose
		(show-env rec!-fact-env (env-frame rec!-fact-env))
		(vl-append
		 line-sep
		 (it "then") (bit "modify") (it "the environment")
		 (it "to fix it up")))
	       (prog4 ".fact  10."))
	 (list (show-env rec!-fact+n-env (env-frame rec!-fact+n-env))
	       (prog4 (regexp-quote fact-body)))
	 (list (show-env rec!-fact+n-env (env-frame rec!-fact+n-env))
	       (prog4 ".fact.-.n, 1.."))
	 (list (rc-superimpose
		(show-env rec!-fact+n+n-env (env-frame rec!-fact+n+n-env))
		(vl-append
		 line-sep
		 (it "an advantage: closure")
		 (it "is only created once")))
	       (prog4 (regexp-quote fact-body)))))
	 
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Modifying Environments"
   (page-item "Nothing in Scheme so far supports" (it "modifying") "a value")
   (page-item "So we need evaluation rules to support" (dt "vectors") "and" (dt "vector update")))

  (slide/title
   "Evaluation of Vector Expressions"
   (page-item "Unlike" (alg-code "cons") "," (alg-code "vector")
	      "does not create a value")
   (page-item "Instead, it's treated like local functions used to be")
   'next
   (alg-code* "..."
	      "(let ([v (vector 1 2 3)]) (vector-ref v 0))"
	      "->"
	      "... (define vec_1743 (vector 1 2 3))"
	      "(let ([v vec_1743]) (vector-ref v 0))"
              "->"
	      "... (define vec_1743 (vector 1 2 3))"
	      "(vector-ref vec_1743 0)"
	      "->"
	      "... (define vec_1743 (vector 1 2 3))"
	      "1"))

  (slide/title/tall
   "Evaluation of Vector Expressions"
   (vl-append
    line-sep
    (page-item "The reason for this definition of"
	       (alg-code "vector")
	       "is to enable" (alg-code "vector-set!"))
    (alg-code* "..."
	       "(let ([v (vector 1 2 3)]) (begin (vector-set! v 0 5) (vector-ref v 0)))"
	       "->"
	       "... (define vec_1743 (vector 1 2 3))"
	       "(let ([v vec_1743]) (begin (vector-set! v 0 5) (vector-ref v 0)))"
	       "->"
	       "... (define vec_1743 (vector 1 2 3))"
	       "(begin (vector-set! vec_1743 0 5) (vector-ref vec_1743 0))"
	       "->"
	       "... (define vec_1743 (vector 5 2 3))"
	       "(vector-ref vec_1743 0)"
	       "->"
	       "... (define vec_1743 (vector 5 2 3))"
	       "5")))
  
  (slide/title
   "Begin Expressions"
   (page-item (alg-code "begin") "evaluates a sequence of expressions, in order")
   (page-item (alg-code "lambda") "and" (alg-code "let")
	      "always supply an implicit" (alg-code "begin"))
   (alg-code* "(let (...) expr_1 ... expr_n)"
	      "= (let (...) (begin expr_1 expr_n))")
   (alg-code* "(lambda (...) expr_1 ... expr_n) "
	      "= (lambda (...) (begin expr_1 expr_n))"))

  (slide/title
   "Changing Recursive Environment Extension"
   (page-para "Now we can change" (tt "extend-env-recursively")
	      "to use" (alg-code "vector-set!"))
   (page-para "Go back to just two datatype variants")
   (prog$* 
    ".vals vector.."
    "(define-datatype environment environment?"
    "  (empty-env-record)"
    "  (extended-env-record"
    "    (syms (list-of symbol?))"
    "    (vals vector?)"
    "    (env environment?))")
   'next
   (page-para* "(implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define xy+f2-env (add-f-closure "x" #f))

  (define xy+f+x-env (add-z "x" xy+f2-env "2" #t))

  (define (prog22 p)
    (prog2-pre "x" "(f y)" p))

  (define (prog23 p)
    (prog2-pre "x" "+((f y), let w = 5 in (f 3))" p))

  (define xy+f+x+w+x-env (add-z "x"
				(env-extension (list "w")
					       (list "5")
					       #f
					       xy+f+x-env
					       (env-frame xy+f2-env))
				"3" #f))

  (define (half-page-para . l)
    (apply para (/ (pict-width titleless-page) 2) l))
  (define (half-page-item . l)
    (apply item (/ (pict-width titleless-page) 2) l))

  (slide/title
   "Back to Lexical Scope"
   'alts
   (list (list (rc-superimpose
		(show-env xy+f+z-env (env-frame xy+f+z-env))
		(half-page-para
		 "What if we change" (alg-code "z") "to" (alg-code "x") "?"))
	       (prog2 "[+].z, y."))
	 (list (rc-superimpose
		(show-env xy+f+x-env (env-frame xy+f+x-env))
		(vl-append
		 font-size
		 (half-page-para
		  "Shape of the environment and location of the argument is unchanged")
		 (half-page-item "argument is always first in first frame")
		 (half-page-item (alg-code "y") "is always second in second frame")))
	       (prog22 "[+].x, y."))
	 (list (rc-superimpose
		(show-env xy+f+x+w+x-env (env-frame xy+f+x+w+x-env))
		(half-page-para "Still true if" (alg-code "f")
				"is called from a more complex environment"))
	       (prog23 "[+].x, y."))))

  (slide/title
   "Compilation"
   (page-para "So why waste time searching the environment on every variable access?")
   'next
   (page-para "A compiler can determine the" (dt "lexical offset") "for each variable statically")
   'next
   (page-para "Terminology:")
   (page-item "A" (dt "compiler") "translates a program from language" (it "X")
	      "to language" (it "Y"))
   (page-item "An" (dt "interpreter") "executes a program in language" (it "X")))

  (slide/title
   "Compilation of Variable Accesses"
   (page-item "We'll write a compiler that transforms")
   (alg-code* "let x = 1  y = 2"
	      "  in let f = proc (x) +(x, y)"
	      "    in (f x)")
   (page-para (ghost bullet) "to")
   (alg-code* "let _ = 1  _ = 2"
	      "  in let _ = proc (_) +(<0,0>, <1,1>)"
	      "    in (<0,0> <1,0>)")
   (page-item "We'll also need an interpreter for the new language"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
