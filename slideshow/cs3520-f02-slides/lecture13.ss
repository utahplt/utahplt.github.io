
(module lecture13 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   (lib "list.ss"))
 
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define wrap-quiz-result values)

  (define (ques n . s)
    (apply page-item
	   (colorize (t (format "Question #~a:" n)) GreenColor)
	   s))

  (define (wrong-ans . s)
    (wrap-quiz-result
     (page-item
      (colorize (t "Wrong answer:")  RedColor)
      s)))

  (define (ans . s)
    (wrap-quiz-result
     (apply page-item
	    (colorize (t "Answer:")  BlueColor)
	    (if (and (= 1 (length s))
		     (string? (car s)))
		(list (bt (car s)))
		s))))

  (slide/title
   "Quiz"
   (ques 1 "What is the value of the following expression?")
   (alg-code "+(1,1)")
   'next
   (blank)
   (wrong-ans (bt "0"))
   'next
   (wrong-ans (bt "42"))
   'next
   (blank)
   (ans (bt "2")))

  (slide/title
   "Quiz"
   (ques 2 "What is the value of the following expression?")
   (alg-code "+ proc 8")
   'next
   (blank)
   (wrong-ans (bt "error"))
   'next
   (blank)
   (ans "Trick question!"
	(alg-code "+ proc 8")
	"is not an expression"))

  (slide/title
   "Language Grammar for Quiz"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "num") (blank)
     (blank)           -or- (alg-code "bool") (blank)
     (blank)           -or- (alg-code "id") (blank)
     (blank)           -or- (alg-code "prim ( {{ expr }}**, )") (blank)
     (blank)           -or- (alg-code "proc (id**,) expr") (blank)
     (blank)           -or- (alg-code "(expr expr**)") (blank)
     (blank)           -or- (alg-code "if expr then expr else expr") (blank)
     (alg-code "prim") eqls (alg-code "+ | - | * | add1 | sub1") (blank)
     (alg-code "bool") eqls (alg-code "true | false") (blank))))

  (slide/title
   "Quiz"
   (ques 3 "Is the following an expression?")
   (alg-code "add1(1, 7)")
   'next
   (blank)
   (wrong-ans (bt "No"))
   'next
   (blank)
   (ans (bt "Yes") "(according to our grammar)"))

  (slide/title
   "Quiz"
   (ques 4 "What is the value of the following expression?")
   (alg-code "add1(1, 7)")
   'next
   (blank)
   (ans (bt "2") "(according to our interpreter)")
   'next
   (blank)
   (wrap-quiz-result
    (page-item "But no" (it "real") "language would accept"
	       (alg-code "add1(1, 7)")))
   'next
   (blank)
   (page-item "Let's agree to call" (alg-code "add1(1, 7)") 
	      "an" (dt "ill-formed expression") " because"
	      (alg-code "add1") "should be used with only"
	      "one argument")
   (page-item "Let's agree to never evaluate"
	      "ill-formed expressions"))

  (slide/title
   "Quiz"
   (ques 5 "What is the value of the following expression?")
   (alg-code "add1(1, 7)")
   'next
   (blank)
   (ans (bt "None") " - the expression is ill-formed"))

  (slide/title
   "Quiz"
   (ques 6 "Is the following a well-formed expression?")
   (alg-code "+(proc(x)x, 5)")
   'next
   (blank)
   (ans "Yes"))

  (slide/title
   "Quiz"
   (ques 7 "What is the value of the following expression?")
   (alg-code "+(proc(x)x, 5)")
   'next
   (blank)
   (ans (bt "None") " - it produces an error:")
   (wrap-quiz-result
    (para* (* 2/3 client-w)
	   "+: expects type <number> as 1st argument, given:"
	   "(closure ((cbv-var x)) (var-exp x) (empty-env-record)); other arguments were: 5"))
   'next
   (blank)
   (page-item "Let's agree that a" (alg-code "proc") "expression cannot be"
	      "inside a" (alg-code "+") "form"))

  (slide/title
   "Quiz"
   (ques 8 "Is the following a well-formed expression?")
   (alg-code "+(proc(x)x, 5)")
   'next
   (blank)
   (ans "No"))

  (slide/title
   "Quiz"
   (ques 9 "Is the following a well-formed expression?")
   (alg-code "+((proc(x)x 7), 5)")
   'next
   (blank)
   (ans "Depends on what we meant by" (it "inside")
	"in our most recent agreement")
   (wrap-quiz-result
    (page-subitem (it "Anywhere inside") " - " (bt "No")))
   (wrap-quiz-result
    (page-subitem (it "Immediately inside") " - " (bt "Yes")))
   'next
   (blank)
   (page-item "Since our intrepreter produces" (bt "12") 
	      ", and since that result makes sense,"
	      "let's agree on" (it "immediately inside")))

  (slide/title
   "Quiz"
   (ques 10 "Is the following a well-formed expression?")
   (alg-code "+((proc(x)x true), 5)")
   'next
   (blank)
   (ans (bt "Yes") ", but we don't want it to be!"))

  (slide/title
   "Quiz"
   (ques 11
	 "Is it possible to define" 
	 (bit "well-formed")
	 "(as a decidable property)"
	 "so that we reject all expressions that produce errors?")
   'next
   (blank)
   (ans (bt "Yes") ": reject" (it "all") "expressions!"))

  (slide/title
   "Quiz"
   (ques 12 
	 "Is it possible to define" 
	 (bit "well-formed")
	 "(as a decidable property)"
	 "so that we reject"
	 (it "only")
	 "expressions that produce errors?")
   'next
   (blank)
   (ans (bt "No"))
   'next
   (blank)
   (wrap-quiz-result
    (alg-code "+(1, if ... then 1 else proc(x)x)"))
   (blank)
   (wrap-quiz-result
    (page-item "If we always knew whether" (alg-code "...") 
	       "produces true or false, we could solve the halting problem")))


  (slide/title
   "Types"
   (page-item "Solution to our dilemma")
   (page-subitem "In the process of rejecting expressions"
		 "that are certainly bad, also reject some"
		 "expressions that are good")
   (alg-code "+(1, if (prime? 131101) then 1 else proc(x)x)")
   'next
   (blank)
   (page-item "Overall strategy:")
   (page-subitem "Assign a" (bit "type") "to each expression"
		 (it "without evaluating"))
   (page-subitem "Compute the type of a complex expression based on the types of its subexpressions"))

  (define type-tree
    (case-lambda
     [(prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append aenv benv cenv eenv wenv close?)
      (type-tree prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append)]
     [(prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append)
      (let ([prog (lambda (s)
		    (cond
		     [(string? s) (alg-code s)]
		     [s s]
		     [else #f]))])
	(let ([pre (prog pre)]
	      [prepre (prog prepre)]
	      [a (prog a)]
	      [sep (prog sep)]
	      [b (prog b)]
	      [sep2 (prog sep2)]
	      [c (prog c)]
	      [post (prog post)]
	      [at atype]
	      [atype (if atype (prog atype) (blank))]
	      [bt btype]
	      [btype (if btype (prog btype) (blank))]
	      [ct ctype]
	      [ctype (and ctype (prog ctype))]
	      [et etype]
	      [etype (prog etype)]
	      [wtype (and wtype (prog wtype))])
	  (let* ([expr (hbl-append pre a sep b (or sep2 (blank)) (or c (blank)) post)]
		 [expr+ (hb-append prepre expr)]
		 [types (let ([ab (hbl-append (* 3 font-size) atype btype)])
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
		    [tline (lambda (whole e t dy)
			     (let-values ([(ec eb) (find-cb whole e)]
					  [(tc tt) (find-ct whole t)]
					  [(w/2) (/ (pict-width e) 2)])
			       (cons-picture
				whole
				`((connect ,tc ,tt ,ec ,(+ eb dy))
				  (connect ,(- ec w/2) ,(+ eb dy) ,(+ ec w/2) ,(+ eb dy))))))])
		(let* ([start (show (show base expr) prepre)]
		       [atype (if at
				  (tline (show start atype)
					 a atype 0)
				  start)]
		       [btype (if bt
				  (tline (show atype btype)
					 b btype 0)
				  atype)]
		       [ctype (if ctype
				  (tline (show btype ctype)
					 c ctype 0)
				  btype)]
		       [all (if et
				(tline (show ctype etype)
				       expr etype -5)
				ctype)])
		  (map list
		       (append
			(list start)
			(if at (list atype) null)
			(if bt (list btype) null)
			(if ct (list ctype) null)
			(if et (list all) null)
			(if wtype 
			    (list (tline (show all wtype) expr+ wtype -10)) 
			    null)))))))))]))

  (define type-tree/formal
    (case-lambda
     [(prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append aenv benv cenv eenv wenv close?)
      (let ([prog (lambda (s)
		    (cond
		     [(string? s) (alg-code s)]
		     [s s]
		     [else #f]))]
	    [typed (lambda (e a t) (let ([j (hbl-append a (alg-code " : ") t)])
				     (if e
					 (hbl-append (alg-code e) (alg-code " |- ") j)
					 j)))])
	(let ([pre (prog pre)]
	      [prepre (prog prepre)]
	      [a (prog a)]
	      [sep (prog sep)]
	      [b (prog b)]
	      [sep2 (prog sep2)]
	      [c (prog c)]
	      [post (prog post)]
	      [at atype]
	      [atype (if atype (prog atype) (blank))]
	      [bt btype]
	      [btype (if btype (prog btype) (blank))]
	      [ct ctype]
	      [ctype (and ctype (prog ctype))]
	      [et etype]
	      [etype (prog etype)]
	      [wtype (and wtype (prog wtype))])
	  (let* ([expr (hbl-append pre a sep b (or sep2 (blank)) (or c (blank)) post)]
		 [expr+ (hb-append prepre expr)])
	    (let ([base (infer (typed eenv expr etype)
			       (apply
				(if close? ante-append/close ante-append)
				(typed aenv a atype)
				(append
				 (if bt
				     (list (typed benv b btype))
				     null)
				 (if ctype
				     (list (typed cenv c ctype))
				     null))))])
	      (list (list
		     (if wtype
			 (infer (typed wenv expr+ wtype) base)
			 base)))))))]
     [(prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append)
      (type-tree/formal prepre pre a sep b sep2 c post atype btype ctype etype wtype tv-append #f #f #f #f #f #f)]))

  (define no-type (colorize (bit "no type") RedColor))

  (define (basic-types type-tree rules)
    (slide/title
     (if rules "Type Rules" "Types")
     (or rules 'nothing)
     (if rules 'next 'nothing)
     (if rules (blank) 'nothing)
     (hbl-append (alg-code "1 : numT"))
     (hbl-append (alg-code "true : boolT"))
     'next
     (blank)
     'alts~
     (type-tree "" "+(" "1" ", " "2" #f #f ")"
		"numT" "numT" #f "numT" #f vc-append)
     'next
     (blank)
     'alts~
     (type-tree "" "+(" "1" ", " "false" #f #f ")"
		"numT" "boolT" #f no-type #f vc-append)))

  (basic-types type-tree #f)
  
  (define int-rule (alg-code "num : numT"))
  (define bool-rule (alg-code "bool : boolT"))
  (define plus-rule (infer (alg-code "+(expr_1, expr_2) : numT")
			   (ante-append
			    (alg-code "expr_1 : numT")
			    (alg-code "expr_2 : numT"))))

  (define basic-rules (hc-append (* 3 font-size)
				 (vl-append
				  font-size
				  int-rule
				  bool-rule)
				 plus-rule))

  (basic-types type-tree/formal basic-rules)

  (slide/title 
   "Type Rules"
   basic-rules
   (blank)
   (infer (alg-code "+(+(1, 2),3) : numT")
	  (ante-append
	   (infer (alg-code "+(1, 2) : numT")
		  (ante-append
		   (alg-code "1 : numT")
		   (alg-code "2 : numT")))
	   (alg-code "3 : numT"))))

  (define (conditional-types type-tree rules)
    (slide/title
     (if rules "Conditional Type Rules" "Types: Conditionals")
     (or rules 'nothing)
     (if rules 'next 'nothing)
     (if rules (blank) 'nothing)
     'alts~
     (type-tree "" "if " "true" " then " "1" " else " "2" ""
		"boolT" "numT" "numT" "numT" #f vc-append)
     'next
     (blank)
     'alts~
     (type-tree "" "if " "+(1,2)" " then " "1" " else " "2" ""
		"numT" "numT" "numT" no-type #f vc-append)
     'next
     (blank)
     'alts~
     (type-tree "" "if " "false" " then " "2" " else " "false" ""
		"boolT" "numT" "boolT" no-type #f vc-append)))

  (conditional-types type-tree #f)

  (define cond-rule (infer (alg-code "if expr_1 then expr_2 else expr_3 : type_0")
			   (ante-append
			    (alg-code "expr_1 : boolT")
			    (alg-code "expr_2 : type_0")
			    (alg-code "expr_3 : type_0"))))

  (conditional-types type-tree/formal cond-rule)
  
  (define shrinker (lambda (l) (map (lambda (a) (map (lambda (x) (scale x 0.8 0.8)) a)) l)))

  (define (variable-types type-tree rules)
    (slide/title
     (if rules
	 "Variable and Function Type Rules"
	 "Types: Variables and Functions")
     (or rules 'nothing)
     (if rules 'next 'nothing)
     (if rules (blank) 'nothing)
     'alts
     ((if rules
	  (lambda (l)
	    (list
	     (list (blank)
		   (page-para "Abbreviations:" (alg-code "Type = type    Expr = expr   Env = env")))
	     l))
	  list)
      (list (hbl-append (alg-code (if rules "{ } |- x : " "x : ")) no-type)))
     'next
     (blank)
     'alts~
     (type-tree "" "proc(boolTd x)" "x" "" "" #f #f ""
		"boolT" #f #f "(*boolT *-> boolT*)" #f vr-append
		"{x : boolT}" #f #f "{ }" #f #f)
     'next
     (blank)
     'alts~
     ((if rules
	  shrinker
	  values)
      (type-tree "proc(boolTd x)" "if " "x" " then " "1" " else " "2" ""
		 "boolT" "numT" "numT" "numT" "(*boolT *-> numT*)" vr-append
		 "{x : boolT}" "{x : boolT}" "{x : boolT}" "{x : boolT}" "{ }" #t))))

  (define var-rule (alg-code "{ ... id : Type ... } |- id : Type"))
  (define func-rule (infer (alg-code "Env |- proc(Type_1 id)Expr : (*Type_1 *-> Type_2*)")
			   (alg-code "{ id : Type_1 } + Env |- Expr : Type_2")))

  (variable-types type-tree #f)
  (variable-types type-tree/formal (vc-append font-size
					      var-rule
					      func-rule))

  (slide/title
   "Resvised Rules"
   (alg-code "Env |- num : numT")
   (alg-code "Env |- bool : boolT")
   (infer (alg-code "Env |- +(Expr_1, Expr_2) : numT")
	  (ante-append
	   (alg-code "Env |- Expr_1 : numT")
	   (alg-code "Env |- Expr_1 : numT")))
   (infer (alg-code "Env |- if Expr_1 then Expr_2 else Expr_3 : Type_0")
	  (ante-append
	   (alg-code "Env |- Expr_1 : boolT")
	   (alg-code "Env |- Expr_2 : Type_0")
	   (alg-code "Env |- Expr_3 : Type_0"))))

  (define (function-call-types type-tree rules)
    (slide/title
     (if rules
	 "Function Call Type Rule"
	 "Types: Function Calls")
     (or rules 'nothing)
     (if rules 'next 'nothing)
     (if rules (blank) 'nothing)
     'alts~
     ((if rules
	  shrinker
	  values)
      (type-tree "" "(" "proc(boolTd x)if x then 1 else 2" "   " "true" #f #f ")"
		 "(*boolT *-> numT*)" "boolT" #f "numT" #f vc-append
		 "{ }" "{ }" #f "{ }" #f #f))
     'next
     (blank)
     'alts~
     ((if rules
	  shrinker
	  values)
      (type-tree "" "(" "proc(boolTd x)if x then 1 else 2" "   " "5" #f #f ")"
		 "(*boolT *-> numT*)" "numT" #f no-type #f vc-append
		 "{ }" "{ }"  #f "{ }" #f #f))
     'next
     (blank)
     'alts~
     (type-tree "" "(" "7" "  " "5" #f #f ")"
		"numT" "numT" #f no-type #f vc-append
		"{ }" "{ }" #f "{ }" #f #f)))

  (define apply-rule (infer (alg-code "Env |- (Expr_1 Expr_2) : Type_3")
			    (ante-append 
			     (alg-code "Env |- Expr_1 : (*Type_2 *-> Type_3*)")
			     (alg-code "Env |- Expr_2 : Type_2"))))

  (function-call-types type-tree #f)
  (function-call-types type-tree/formal apply-rule)

  (slide/title/tall
   "Types: Multiple Arguments"
   'alts~
   (type-tree "proc(numTd x, numTd y)" "+(" "x" ", " "y" #f #f ")"
	      "numT" "numT" #f "numT" "(*numT *X* numT *-> numT*)" vr-append)
   'next
   (blank)
   'alts~
   (type-tree "" "(" "proc(numTd x, numTd y)+(x, y)" "   " "5" " " "6" ")"
	      "(*numT *X* numT *-> numT*)" "numT" "numT" "numT" #f vc-append)
   'next
   (blank)
   'alts~
   (type-tree "" "(" "proc(numTd x, numTd y)+(x, y)" "   " "5" #f #f ")"
	      "(*numT *X* numT *-> numT*)" "numT" #f no-type #f vc-append))

  (define func-rule/multi
    (infer (alg-code "Env |- proc(Type_1 id_1, ... Type_n id_n)Expr : (*Type_1 *X* .... Type_n *-> Type_0*)")
	   (alg-code "{ id_1 : Type_1, .... id_n : Type_n } + Env |- Expr : Type_0")))
  (define apply-rule/multi
    (infer (alg-code "Env |- (Expr_0 Expr_1 .... Expr_n) : Type_0")
	   (ante-append 
	    (alg-code "Env |- Expr_0 : (*Type_1 *X* .... Type_n *-> Type_0*)")
	    (alg-code "Env |- Expr_1 : Type_1")
	    (alg-code "....")
	    (alg-code "Env |- Expr_n : Type_n"))))

  (slide/title
   "Revised Function and Call Rules"
   func-rule/multi
   (blank)
   apply-rule/multi)

  (slide/title
   "New Interpreter and Checker"
   (page-itemize "Change our interpreter:")
   (page-subitem "Add types for arguments and letrec results to the grammar")
   (page-itemize "Implement a type-checker:")
   (page-subitem "Produces the same type that the rules procedure")
   (page-subitem "Calls itself recusrively to get types for sub-expressions")
   (page-subitem "Treat primitives as built-in functions")
   (alg-code "+ : (*numT *X* numT *-> numT*)"))

  'done)
