
(module lecture1 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Original Grammar for Algebra Programs"
   complete-table
   'next
   (page-item "Change step-by-step to Scheme"))

  (define scheme-expr-lines
    (list (alg-code "expr") eqls (alg-code "(+ expr expr)") (blank)
	  (blank)           -or- (alg-code "(- expr expr)") (blank)
	  (blank)           -or- (alg-code "(id expr)") (blank)
	  (blank)           -or- (alg-code "id | num") (blank)))

  (define scheme-defn-line
    (list (alg-code "defn") eqls (alg-code "(define (id id) expr)")  (blank)))

  (slide/title
   "Change #1: Move Parens and Add \"define\""
   (grammar-table
    (append
     prog-line
     scheme-defn-line
     scheme-expr-lines
     (list (blank) (blank) (blank)  (blank))
     id+n-lines))
   'next
   'alts
   (list (list (page-item (alg-code "+") "and" (alg-code "-") "moved to initial position"))
	 (list (page-item "Open parenthesis for function call moved to before the function name"))
	 (list (page-item "Definition use the" (alg-code "define") "keyword, and parenthesis moved"))
	 (list (hc-append
		(* 3 font-size)
		(vl-append
		 line-sep
		 (alg-code "f(x) = (x + 1)")
		 (alg-code "f((2 + 3))"))
		(colorize (arrow font-size 0) OrangeColor)
		(vl-append
		 line-sep
		 (alg-code "(define (f x) (+ x 1))")
		 (alg-code "(f (+ 2 3))"))))))

  (slide/title
   "Change #2: Generalize + and -, Add Primitives"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "(+ expr**)") (blank)
     (blank)           -or- (alg-code "(- expr++)") (blank)
     (blank)           -or- (alg-code "(~* expr**)") (blank)
     (blank)           -or- (alg-code "(/ expr++)") (blank)
     (blank)           -or- (alg-code "(modulo expr expr)") (blank)
     (blank)           -or- (alg-code "(expt expr expr)") (blank)
     (blank)           -or- (alg-code "...") (blank)))
   'next
   'alts
   (list (list (alg-code "(+ 1 2 3 7)  ->  13"))
	 (list (alg-code "(+)  ->  0"))
	 (list (alg-code "(- 1)  ->  -1"))
	 (list (alg-code "(~*~)  ->  1"))
	 (list (alg-code "(/ 2)  ->  1/2"))
	 (list (alg-code "(expt 13 20)  ->  19004963774880799438801"))))

  (define scheme-defn*-line
    (list (alg-code "defn") eqls (alg-code "(define (id id**) expr)")  (blank)))

  (slide/title
   "Change #3: Generalize Defined Functions"
   (grammar-table
    (append
     scheme-defn*-line
     (list
      (alg-code "expr") eqls (alg-code "...") (blank)
      (blank)           -or- (alg-code "(id expr**)") (blank))))
   'next
   (blank)
   'alts
   (list (list (two-line-reduction
		(alg-code "(define (f a b) (+ a b))")
		(alg-code "(f 1 2)")
		(alg-code "3")))
	 (list (beta*-reduction #f #f))))

  (slide/title
   "Change #4: Add Booleans"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "bool") (blank)
     (blank)           -or- (alg-code "(and expr**)") (blank)
     (blank)           -or- (alg-code "(or expr**)") (blank)
     (blank)           -or- (alg-code "(zero? expr)") (blank)
     (blank)           -or- (alg-code "...") (blank)
     (alg-code "bool") eqls (alg-code "#f") (blank)
     (blank)           -or- (alg-code "#t") (blank)))
   'next
   'alts
   (list (list (alg-code "(and #t #f)  ->  #f"))
	 (list (alg-code "(zero? 1)  ->  #f"))
	 (list (alg-code "(zero? 0)  ->  #t"))))

  (slide/title
   "Change #5: Add Symbols"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "symbol") (blank)
     (blank)           -or- (alg-code "(eq? expr expr)") (blank)
     (blank)           -or- (alg-code "...") (blank)
     (alg-code "symbol") eqls (alg-code "'id") (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code "(eq? 'a 'a)  ->  #t"))
	 (list (alg-code "(eq? 'a 'b)  ->  #f"))))

  (slide/title
   "Change #6: Add Conditionals"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "(cond cond-line**)") (blank)
     (alg-code "cond-line") eqls (alg-code "[expr expr]") (blank)
     (blank)                -or- (alg-code "[else expr]") (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code "(cond [#t 1])  ->  1"))
	 (list (alg-code "(cond [#f 1] [#t 2])  ->  (cond [#t 2])")
	       'next
	       (alg-code "(cond [#t 2])  ->  2"))
	 (list (alg-code "(cond [#t 1] [#t 2])  -> 1"))
	 (list (alg-code "(cond [#f 1] [else 5])  -> ... -> 5"))
	 (list (alg-code "(cond [(zero? 5) 1] [else 2])  ->  (cond [#f 1] [else 2])")
	       'next
	       (alg-code "(cond [#f 1] [else 2])  ->  (cond [else 2])  ->  2"))))

  (slide/title
   "Change #7: Values"
   (page-para "While extending the set of expressions, we've also extended the set of" (dt "values") ":")
   (grammar-table
    (list (alg-code "val") eqls (alg-code "num") (blank)
	  (blank)          -or- (alg-code "bool")  (blank)
	  (blank)          -or- (alg-code "symbol")  (blank)))
   'next
   'alts
   (let ([prog (alg-code "(define (f x) (+ x 1))")])
     (list (list (page-item "Evaluation stops when it reaches a member of" (alg-code "val"))
		 'next
		 (page-item "Function application requires a" (alg-code "val")))
	   (list (two-line-reduction
		  prog
		  (alg-code "(f (+ 1 2))")
		  (alg-code "(f 3)"))
		 'next
		 (htl-append 
		  (* 2 font-size)
		  (colorize (bit "not") MetaColor)
		  (vl-append
		   line-sep
		   prog
		   (alg-code "(+ (+ 1 2) 1)"))))
	   (list (beta*-reduction #t #f)))))
		  

  (slide/title
   "Change #8: Add Pairs"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "(cons expr expr)") (blank)
     (blank)           -or- (alg-code "(car expr)") (blank)
     (blank)           -or- (alg-code "(cdr expr)") (blank)
     (alg-code "val") eqls (alg-code "num | bool | symbol") (blank)
     (blank)          -or- (alg-code "(cons val val)")  (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code "(cons 1 2) isin? val")
	       'next
	       (bt "yes"))
	 (list (alg-code "(cons 1 (cons 2 (cons 3 4))) isin val"))
	 (list (alg-code "(cons 1 (+ 1 1))  isin? val")
	       'next
	       (bt "no")
	       (alg-code "(cons 1 (+ 1 1))  ->  (cons 1 2)"))
	 (list (alg-code "(car (cons 1 2)) isin? val")
	       (bt "no")
	       (alg-code "(car (cons 1 2))  ->  1"))
	 (list (page-para "More generally:")
	       (alg-code "(car (cons ~X ~Y))  ->  ~X")
	       'next
	       (alg-code "(cdr (cons ~X ~Y))  ->  ~Y"))
	 (list (page-para "Also:")
	       (alg-code "(cadar (cons (cons ~X (cons ~Y ~Z)) ~W))  ->  ~Y")
	       'next
	       (page-para* "(or any combination of up to four \"a\"s and \"d\"s)"))))


  (slide/title
   "Change #9: Add the Empty List"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "...") (blank)
     (blank)           -or- (alg-code "'()") (blank)
     (alg-code "val") eqls (alg-code "num | bool | symbol") (blank)
     (blank)          -or- (alg-code "(cons val val) | '()")  (blank)
     (alg-code "lst") eqls (alg-code "'()") (blank)
     (blank)          -or- (alg-code "(cons val lst)") (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code "'() isin? lst")
	       'next
	       (bt "yes"))
	 (list (alg-code "(cons 1 '()) isin? lst")
	       'next
	       (bt "yes"))
	 (list (alg-code "(cons '() 1) isin? lst")
	       'next
	       (bt "no"))
	 (list (alg-code "(cons 1 (cons 2 (cons 3 '()))) isin? lst")
	       'next
	       (bt "yes"))
	 (list (page-para* "Is every" (alg-code "lst") "a" (alg-code "val") "?")
	       'next
	       (bt "yes"))))

  (slide/title
   "List Shortcuts"
   (page-item (alg-code "(list 1 2 3) = (cons 1 (cons 2 (cons 3 '())))"))
   'next
   (page-item (alg-code "'(1 2 3) = (cons 1 (cons 2 (cons 3 '())))"))
   'next
   (page-item (alg-code "'(a b c) = (cons 'a (cons 'b (cons 'c '())))"))
   'next
   (page-item (vl-append
	       line-sep
	       (alg-code "'(a (1 2) c)")
	       (alg-code "= (list 'a '(1 2) 'c)")
	       (alg-code "= (cons 'a (cons (cons 1 (cons 2 '())) (cons 'c '())))"))))
  
  (slide/title
   "List Predicates"
   (page-item (alg-code "(null? '()) = #t"))
   'next
   (page-item (alg-code "(null? (cons 1 2)) = #f"))
   'next
   (page-item (alg-code "(pair? '()) = #f"))
   (page-item (alg-code "(pair? (cons 1 2)) = #t"))
   'next
   (page-item (alg-code "(list? '()) = #t"))
   (page-item (alg-code "(list? (cons 1 2)) = #f"))
   (page-item (alg-code "(list? (cons 1 '())) = #t")))

  (slide/title
   "Change #10: Functions Are Values"
   (grammar-table
    (list
     (alg-code "val") eqls (alg-code "...") (blank)
     (blank)          -or- (alg-code "defined-id")  (blank)
     (alg-code "defined-id") eqls (page-para* "an" (alg-code "id") "that is" 
					      (hbl-append (alg-code "define") (t "d"))) (blank)))
   'next
   (blank)
   'alts
   (let ([defs (vl-append
		line-sep
		(alg-code "(define (twice f x) (f (f x)))")
		(alg-code "(define (g y) (+ y 1))"))])
     (list (list (two-line-reduction
		  defs
		  (alg-code "(twice g 0)")
		  (alg-code "(g (g 0))")))
	   (list (two-line-reduction
		  defs
		  (alg-code "(g (g 0))")
		  (alg-code "(g (+ 0 1))")))
	   (list (two-line-reduction
		  defs
		  (alg-code "(g (+ 0 1))")
		  (alg-code "(g 1)")))
	   (list (two-line-reduction
		  defs
		  (alg-code "(g 1)")
		  (alg-code "(+ 1 1)")))
	   (list (two-line-reduction
		  defs
		  (alg-code "(+ 1 1)")
		  (alg-code "2"))))))

  (slide/title
   "Alternate Notations (Used by the Book)"
   (page-item (vl-append
	       line-sep
	       (alg-code "(define id_0 (lambda (id_1....id_k) expr))")
	       (alg-code "= (define (id_0 id_1....id_k) expr)")))
   (page-item (vl-append
	       line-sep
	       (alg-code "(if expr_1 expr_2 expr_3)")
	       (alg-code "= (cond [expr_1 expr_2] [else expr_3])"))))
	      
  (slide/title
   "Stuff You Don't Need (Yet)"
   (page-item "local bindings")
   (page-item (bt "set!"))
   (page-item (bt "begin") "(including implicit)")
   (page-item "I/O primitives")
   (page-item "vectors")
   (page-item (bt "do") "(never need this one)"))

  'done)
