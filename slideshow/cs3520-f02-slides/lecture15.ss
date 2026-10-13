
(module lecture15 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/ttree.ss"
	   (lib "list.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (last l)
    (let loop ([l l])
      (if (null? (cdr l))
	  (car l)
	  (loop (cdr l)))))
  
  (define empty-env "{ }")

  (define (example-slide what factor l . rest)
    (define (type lit env exp t)
      (let ([p (hbl-append (alg-code (if (list? env) (car env) env))
			   (lit (alg-code " |- "))
			   exp (lit (alg-code " : "))
			   t)])
	(if (= factor 1.0)
	    p
	    (scale p factor factor))))
    (apply
     slide/title
     (format "Typing Example: ~a" what)
     (blank)
     'alts~
     (let loop ([l l])
       (let ([prepre (and (list? (car l))
			  (type ghost (car l)
				(ghost (alg-code (cadr l)))
				(ghost (alg-code (caddr l)))))]
	     [pre (type values (car l)
			(alg-code (cadr l))
			(ghost (alg-code (caddr l))))]
	     [post (type values (car l)
			 (alg-code (cadr l))
			 (alg-code (caddr l)))]
	     [equate (let ([e (cadddr l)])
		       (if e (alg-code e) #f))])
	 (if (null? (cddddr l))
	     (let ([l (list (list pre)
			    (if equate
				(list post equate)
				(list post)))])
	       (if prepre
		   (cons (list prepre) l)
		   l))
	     (let ([antes (map loop (cddddr l))])
	       (append
		(let ([mk (lambda (pre)
			    (let ([whole (infer pre
						(apply
						 ante-append
						 (map car (map last antes))))])
			      (let-values ([(x y) (find-lb whole pre)])
				(list (cons-picture
				       (ghost (launder whole))
				       `((place ,x ,y ,pre)))))))])
		  (if prepre
		      (list (mk prepre) (mk pre))
		      (list (mk pre))))
		(let iloop ([antes antes][done null][done-equates null])
		  (if (null? antes)
		      (list (append (list (infer post
						 (apply ante-append done)))
				    done-equates
				    (if equate (list equate) null)))
		      (append (map (lambda (stage)
				     (append
				      (list (infer pre
						   (apply
						    ante-append
						    (append
						     done
						     (list (car stage))
						     (map ghost (map car (map last (cdr antes))))))))
				      done-equates
				      (cdr stage)))
				   (car antes))
			      (iloop (cdr antes) 
				     (append done
					     (list (car (last (car antes)))))
				     (append done-equates
					     (cdr (last (car antes)))))))))))))
     rest))

  (example-slide "Number" 1.0
		 (list empty-env "5" "intT" #f)
		 'next
		 (blank)
		 (page-para "Each")
		 (alg-code "Env |- Expr : Type") 
		 (page-para "is a call to" 
			    (tt "type-of-expression")
			    "with arguments" (alg-code "Expr") "and" (alg-code "Env")
			    "where the result is" (alg-code "Type")))

  (example-slide "Sum" 1.0
		 (list empty-env "+(1,2)" "intT" #f
		       (list empty-env "1" "intT" #f)
		       (list empty-env "2" "intT" #f))
		 'next
		 (blank)
		 (page-item "Actually, the type checker treats primitives like functions,"
			    "but it could be checked directly as above")
		 (page-item "The above strategy is a good one for HW7, because primitive"
			    "checking is different than function checking"))

  (example-slide "Function" 1.0
		 (list empty-env "proc(intTd x) +(x,2)" "(*intT *-> intT*)" #f
		       (let ([x-env "{ x : intT }"])
			 (list (list x-env) "+(x,2)" "intT" #f
			       (list x-env "x" "intT" #f)
			       (list x-env "2" "intT" #f)))))

  (example-slide "Function Call" 1.0
		 (list empty-env "(proc(intTd x)x  12)" "T_2" "(*intT *-> intT*) = (*intT *-> T_2*)"
		       (list empty-env "proc(intTd x)x" "(*intT *-> intT*)" #f
			     (let ([x-env "{ x : intT }"])
			       (list (list x-env) "x" "intT" #f)))
		       (list empty-env "12" "intT" #f))
		 'next
		 (blank)
		 (alg-code "simplified: intT")
		 'next
		 (blank)
		 (page-item "For inference, create a new type variable for each application"))

  
  (example-slide "? Argument" 1.0
		 (list empty-env "proc(? x) +(x,2)" "(*T_1 *-> intT*)" #f
		       (let ([x-env "{ x : T_1 }"])
			 (list (list x-env) "+(x,2)" "intT" "T_1 = intT"
			       (list x-env "x" "T_1" #f)
			       (list x-env "2" "intT" #f))))
		 'next
		 (blank)
		 (alg-code "simplified: (*intT *-> intT*)")
		 'next
		 (blank)
		 (page-item "Create a new type variable for each" (alg-code "?")))

  (example-slide "? Argument" 0.8
		 (list empty-env "proc(? x) if x then 2 else 3" "(*T_1 *-> intT*)" #f
		       (let ([x-env "{ x : T_1 }"])
			 (list (list x-env) "if x then 2 else 3" "intT" "T_1 = boolT"
			       (list x-env "x" "T_1" #f)
			       (list x-env "2" "intT" #f)
			       (list x-env "3" "intT" #f))))
		 'next
		 (blank)
		 (alg-code "simplified: (*boolT *-> intT*)"))

  (example-slide "Function-Calling Function" 1.0
		 (list empty-env "proc(? f)(f 12)" "(*T_1 *-> T_2*)" #f
		       (let ([f-env "{ f : T_1 }"])
			 (list (list f-env) "(f 12)" "T_2" "T_1 = (*intT *-> T_2*)"
			       (list f-env "f" "T_1" #f)
			       (list f-env "12" "intT" #f))))
		 'next
		 (blank)
		 (alg-code "simplified: (*(*intT *-> T_2*) *-> T_2*)"))

  (example-slide "Identity" 1.0
		 (list empty-env "proc(? x) x" "(*T_1 *-> T_1*)" #f
		       (let ([x-env "{ x : T_1 }"])
			 (list (list x-env) "x" "T_1" #f)))
		 'next
		 (blank)
		 (it "no simplification possible"))

  (example-slide "Identity Applied" 1.0
		 (list empty-env "(proc(? x)x  false)" "T_2" "(*T_1 *-> T_1*) = (*boolT *-> T_2*)"
		       (list empty-env "proc(? x) x" "(*T_1 *-> T_1*)" #f
			     (let ([x-env "{ x : T_1 }"])
			       (list (list x-env) "x" "T_1" #f)))
		       (list empty-env "false" "boolT" #f))
		 'next
		 (blank)
		 (alg-code "simplfied: boolT"))

  (example-slide "Function-Making Function" 1.0
		 (list empty-env "proc(? x) proc(? y) x" "(*T_1 *-> (*T_2 *-> T_1*)*)" #f
		       (let ([x-env "{ x : T_1 }"])
			 (list (list x-env) "proc(? y) x" "(*T_2 *-> T_1*)" #f
			       (let ([xy-env "{ x : T_1, y : T_2 }"])
				 (list (list xy-env) "x" "T_1" #f)))))
		 'next
		 (blank)
		 (it "no simplification possible"))

  '(example-slide "Function-Calling Function" 0.8
		  (list empty-env "proc(? f)+(1,(f 12))" "(*T_1 *-> intT*)" #f
			(let ([f-env "{ f : T_1 }"])
			  (list (list f-env) "+(1,(f 12))" "intT" "T_2 = intT"
				(list f-env "1" "intT" #f)
				(list f-env "(f 12)" "T_2" "T_1 = (*intT *-> T_2*)"
				      (list f-env "f" "T_1" #f)
				      (list f-env "12" "intT" #f)))))
		  'next
		  (blank)
		  (alg-code "simplified: (*(*intT *-> intT*) *-> intT*)"))

  (example-slide "Compound Primitive Data" 1.0
		 (list empty-env "cons(1,2)" "([*intT *:* intT*])" #f
		       (list empty-env "1" "intT" #f)
		       (list empty-env "2" "intT" #f))
		 'next
		 'alts
		 (list (list
			(page-item "In general," (alg-code "([*T_1 *:* T_2*])") "means"
				   "a pair whose first element is of type" (alg-code "T_1")
				   "and second element is of type" (alg-code "T_2"))
			(page-item "More conventional notation is"  (alg-code "(*T_1 *X* T_2*)")))
		       (list
			(page-para "General rule:")
			(infer (alg-code "Env |- cons(Expr_1, Expr_2) : ([*Type_1 *:* Type_2*])")
			       (ante-append
				(alg-code "Env |- Expr_1 : Type_1")
				(alg-code "Env |- Expr_2 : Type_2"))))))

  (example-slide "Compound Primitive Data" 1.0
		 (list empty-env "car(cons(1,2))" "intT" #f
		       (list empty-env "cons(1,2)" "([*intT *:* intT*])" #f))
		 'next
		 (page-para "General rule:")
		 (infer (alg-code "Env |- car(Expr) : Type_1")
			(ante-append
			 (alg-code "Env |- Expr : ([*Type_1 *:* Type_2*])")))
		 (infer (alg-code "Env |- cdr(Expr) : Type_2")
			(ante-append
			 (alg-code "Env |- Expr : ([*Type_1 *:* Type_2*])"))))

  (slide/title
   "Infinite Loops"
   (page-para "What if we extend the language with a special" (alg-code "Omega")
	      "expression that loops forever?")
   (page-item (alg-code "if true then 1 else Omega  ->>  1"))
   (page-item (alg-code "if false then 1 else Omega  ->> ") (colorize (it "loops forever") BlueColor))
   (page-item (alg-code "if true then proc(? x)x else Omega  ->>  proc(? x)x"))
   'next
   (blank)
   (page-para "What is the type of" (alg-code "Omega") "?")
   (colorize (page-para* "For HW7, it's" (alg-code "intT") ", but more generally...")
	     BlueColor))

  (example-slide "Infinite Loop" 1.0
		 (list empty-env "if true then 1 else Omega" "intT" "T_1 = intT"
		       (list empty-env "true" "boolT" #f)
		       (list empty-env "1" "intT" #f)
		       (list empty-env "Omega" "T_1" #f))
		 'next
		 (blank)
		 (page-item "Create a new type variable for each" (alg-code "Omega")))


  (slide/title
   "Type Inference Summary"
   (page-item "New type variable for each" (alg-code "?"))
   (page-item "New type variable for each application")
   (page-item "New type variable for each" (alg-code "Omega"))
   (page-item "Checking a type equation can force a type variable to match a certain type"))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (pie type? typed-mode)
    (let ([size (* font-size 8)])
      (dc (lambda (dc x y)
	    (let ([b (send dc get-brush)]
		  [p (send dc get-pen)]
		  [set-brush (lambda (c)
			       (send dc set-brush 
				     (send the-brush-list
					   find-or-create-brush
					   c 'solid)))])
	      (send dc set-pen (send the-pen-list
				     find-or-create-pen
				     "white" 1 'transparent))

	      (set-brush red)
	      (send dc draw-ellipse x y size size)
	      (set-brush blue)
	      (send dc draw-arc x y size size 0 (* 1/2 pi))
	      (set-brush green)
	      (send dc draw-arc x y size size (* 1/2 pi) pi)
	      (when type?
		(set-brush orange)
		(send dc draw-arc x y size size pi (* 5/4 pi)))
	      (when typed-mode
		(send dc set-brush (send the-brush-list
					 find-or-create-brush
					 "white" 'cross-hatch))
		(let ([s2 (if (eq? typed-mode 'wide)
			      (* 3/4 size)
			      (* 1/2 size))])
		  (send dc draw-arc 
			(+ x (/ (- size s2) 2)) (+ y (/ (- size s2) 2)) s2 s2
			0 (* (cond 
			      [(eq? typed-mode 'tiny) 1/2]
			      [(memq typed-mode '(small wide)) 1]
			      [else 5/4])
			     pi))))
	      
	      (send dc set-brush b)
	      (send dc set-pen p)))
	  size size 0 0)))

  (define (universe type? typed-mode)
    (let* ([pwidth (* font-size 8)]
	   [para (lambda s (para* pwidth s))]
	   [para/r (lambda s (para*/r pwidth s))]
	   [para/c (lambda s (para*/c pwidth s))]
	   [crash (colorize (para/c "programs that crash") red)]
	   [type (colorize (para/c "programs that crash on" (bt "types")) red)]
	   [variant (colorize (para/r "programs that crash on" (bt "variants")) orange)]
	   [value (colorize (para "programs that produce values") blue)]
	   [forever (colorize (para/r "programs that run forever") green)]
	   [welltyped (para "well-typed programs")]
	   [pie (pie type? typed-mode)]
	   [centermark (blank (/ (pict-width pie) 3))]
	   [together (ht-append font-size 
				(vr-append
				 (max 0 (- (pict-height pie)
					   (+ (pict-height forever)
					      (pict-height variant))))
				 forever
				 ((if type? values ghost) variant))
				(vc-append
				 font-size
				 (cc-superimpose
				  pie
				  centermark)
				 (if type? 
				     type 
				     (ct-superimpose
				      (ghost type)
				      crash)))
				(vl-append
				 (max 0 (- (pict-height pie)
					   (+ (pict-height value)
					      (pict-height welltyped))))
				 value
				 ((if typed-mode values ghost) welltyped)))])
      (if typed-mode
	  (add-line together
		    welltyped find-lc
		    centermark find-rc
		    2)
	  together)))

  (slide/title
   "The Universe of Programs"
   (page-item "The goal of type-checking is to rule out bad programs")
   (alg-code  "+(1, true)")
   (page-item "Unfortunately, some good programs will be ruled out, too")
   (alg-code  "+(1, if true then 1 else false)"))

  (slide/title
   "The Universe of Programs"
   'alts
   (list (list (universe #f #f)
	       (page-item "Every program falls into one of three categories"))
	 (list (universe #f 'small)
	       (page-item "The idea is that a type checker rules out the error category"))
	 (list (universe #f 'large)
	       (page-item "But a type checker for most languages will allow some errors!")
	       (blank)
	       (alg-code  "1 / 0 ->> divide by zero"))
	 (list (universe #t 'large)
	       (page-item "Still, a type checker" (it "always") "rules out a certain class of errors")
	       (page-subitem "Division by 0 is a" (dt "variant error")))
	 (list (universe #f 'small)
	       (page-item "Our language happens to have no variant errors, so the type checker rules out all errors"))
	 (list (universe #f 'tiny)
	       (page-item "In fact, if we get rid of" (alg-code  "letrec") 
			  ", then every well-typed program terminates with a value!"))))

  (define (sub a b)
    (hbl-append (text a `(bold . ,main-font) font-size)
		(colorize (text b `(bold subscript . ,main-font) font-size) BlueColor)))
  
  (define (-> a b)
    (hbl-append (alg-code "(*")
		a
		(alg-code " *-> ")
		b
		(alg-code "*)")))
  
  (define self-app (type-tree "proc(?_1 x)" "(" "x" " " "x" #f #f ")"
			      (sub "T" "1") (sub "T" "1") #f
			      (colorize
			       (hbl-append
				(colorize (bit "no type: ") red)
				(sub "T" "1")
				(t " can't be ")
				(->
				 (sub "T" "1")
				 (sub "T" "2")))
			       "black")
			      #f vr-append))

  (slide/title
   "Intution for Termination"
   'alts
   (list
    (list
     (page-para "Recall that to get rid of" (alg-code  "letrec"))
     (alg-code*
      "letrec int sum = proc(int x)"
      "                   if zero?(x)"
      "                      then 0"
      "                      else +(x,(sum -(x, 1)))"
      "  in (sum 10)")
     (page-para "we can use self-application:")
   (alg-code*
    "let sum = proc(int x, ? sum)"
    "            if zero?(x)"
    "               then 0"
    "               else +(x,((sum sum) -(x, 1)))"
    "  in ((sum sum) 10)"))
    (list
     (page-para "But we've already seen that we can't type self-application:")
     (blank)
     (car (last self-app))
     (blank)
     (page-para "The only way around this restriction is to restore"
		(alg-code  "letrec")
		"or extend the type language.")
     (blank)
     (page-para "(Extending the type language in this direction is beyond the scope of the course.)"))))
  
  (slide/title
   "The Universe of Programs"
   (page-item "There are other ways that we'd like to"
	      "expand the set of well-formed programs")
   (blank)
   'alts
   (list (list (universe #f 'small))
	 (list (universe #f 'wide)
	       (page-item "Adjusting the type rules can allow more programs"))))

  (define (t->t n)
    (alg-code (format "(*T_~a *-> T_~a*)" n n)))
  
  (define poly
    (type-tree "" "proc(?_1 y)" "y" "" #f #f #f ""
	       (sub "T" "1") #f #f
	       (t->t "1")
	       #f
	       vr-append))
  
  (define (mk-poly-ex poly?)
    (let ([tree
	   (type-tree " in " "if (" "f" " true) then (" "f" " 1) else (" "f" " 0)"
		      (t->t (if poly? "2" "1"))
		      (t->t (if poly? "3" "1"))
		      (t->t (if poly? "4" "1"))
		      (if poly? 
			  "int" 
			  (hbl-append
			   (colorize (bit "no type: ") red)
			   (sub "T" "1")
			   (t " can't be both ")
			   (alg-code "boolT")
			   (t  " and ")
			   (alg-code "intT")))
		      #f vc-append)])
      (map
       (lambda (l)
	 (list (vl-append
		(hbl-append (alg-code "let f = proc(?_1 y)y")
			    ((if (and (eq? l (car tree)) 
				      (not poly?))
				 ghost 
				 values)
			     (hbl-append 
			      (alg-code " : ")
			      (t->t "1"))))
		(car l))))
       tree)))

  (slide/title
   "Polymorphism"
   (car (last poly))
   'next
   (blank)
   'alts~
   (mk-poly-ex #f))
 
  (slide/title
   "Polymorphism"
   (page-item "New rule: when type-checking the use of a let-bound variable,"
	      "create fresh versions of unconstrained type variables")
   (blank)
   'alts~
   (mk-poly-ex #t)
   (let ([= (colorize (t " = ") RedColor)])
     (hbl-append (sub "T" "2") = (alg-code "boolT")
		 (blank font-size)
		 (sub "T" "3") = (alg-code "intT")
		 (blank font-size)
		 (sub "T" "4") = (alg-code "intT")))
   (blank)
   (page-item "This rule is called" (dt "let-based") (dt "polymorphism")))
  

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
