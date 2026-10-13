
(module lecture14 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   (lib "list.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title/tall
   "Current Book Language"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "num") (blank)
     (blank)           -or- (alg-code "true | false") (blank)
     (blank)           -or- (alg-code "id") (blank)
     (blank)           -or- (alg-code "prim ( {{ expr }}**, )") (blank)
     (blank)           -or- (alg-code "proc ({{ tyexpr id }}**,) expr") (blank)
     (blank)           -or- (alg-code "(expr expr**)") (blank)
     (blank)           -or- (alg-code "if expr then expr else expr") (blank)
     (blank)           -or- (alg-code "let {{ id = expr }}** in expr") (blank)
     (blank)           -or- (alg-code "letrec {{ tyexpr id({{ tyexpr id }}**,)") (blank)
     (blank)        (blank) (alg-code "               = expr }}**") (blank)
     (blank)        (blank) (alg-code "  in expr") (blank)
     (t " ") (blank)(blank)(blank)
     (alg-code "tyexpr") eqls (alg-code "intTd") (blank)
     (blank)           -or- (alg-code "boolTd") (blank)
     (blank)           -or- (alg-code "(tyexpr _> tyexpr)") (blank))))

  (slide/title/tall
   "Types versus Type Expressions"
   (table 3
	  (list 
	   (alg-code "tyexpr") (blank) (alg-code "type")
	   (alg-code "intTd") (t "expands to") (alg-code "intT")
	   (alg-code "boolTd") (t "expands to") (alg-code "boolT")
	   (alg-code "(boolTd _> intTd)") (t "expands to") (alg-code "(*boolT *-> intT*)")
	   (blank) (t "etc.") (blank))
	  cc-superimpose cc-superimpose font-size (cons (/ 2 font-size) line-sep))
   'next
   (page-para "Datatype for types:")
   (prog*
    "(define-datatype"
    " type type?"
    " (atomic-type (name symbol?))"
    " (proc-type (arg-types (list-of type?))"
    "            (result-type type?)))"
    " "
    "(define int-type (atomic-type 'int))"
    "(define bool-type (atomic-type 'bool))"))

  (slide/title
   "Implementing a Type Checker"
   (prog* 
    ";; type-of-expression : expr tenv -> type"
    ";; signals an error if no type for exp"
    ";;"
    "(define (type-of-expression exp tenv)"
    "  (cases expression exp"
    "    (lit-exp ...)"
    "    (true-exp ...)"
    "    (false-exp ...)"
    "    (var-exp ...)"
    "    (primapp-exp ...)"
    "    (proc-exp ...)"
    "    (app-exp ...)"
    "    (if-exp ...)"
    "    (let-exp ...)"
    "    (letrec-exp ...)))"))

  (define (i what)
    (format "Implementation: ~a case" what))

  (slide/title
   (i "lit-exp")
   (page-item "Example:")
   (alg-code "5")
   (page-item "The rule from previous lecture:")
   (alg-code "Env |- num : intT")
   (page-item "In Scheme:")
   (prog*
    "(lit-exp (n) int-type)"))

  (slide/title
   (i "true-exp and false-exp")
   (page-item "Example:")
   (alg-code "true")
   (page-item "The rule from previous lecture:")
   (alg-code "Env |- bool : boolT")
   (page-item "In Scheme:")
   (prog*
    "(true-exp () bool-type)"
    "(false-exp () bool-type)"))

  (slide/title
   (i "var-exp")
   (page-item "Example:")
   (alg-code "... x ...")
   (page-item "The rule from previous lecture:")
   (alg-code "{ ... id : Type ... } |- id : Type")
   (page-item "In Scheme:")
   (prog*
    "(var-exp (id) (apply-tenv tenv id))"
    ";; where apply-tenv signals an error"
    ";; if id is not in tenv"))
	 
  (slide/title/tall
   (i "if-exp")
   (alg-code "if true then 5 else +(1,2)")
   (page-item "The rule from previous lecture:")
   (infer (alg-code "Env |- if Expr_1 then Expr_2 else Expr_3 : Type_0")
	  (ante-append
	   (alg-code "Env |- Expr_1 : boolT")
	   (alg-code "Env |- Expr_2 : Type_0")
	   (alg-code "Env |- Expr_3 : Type_0")))
   (page-item "In Scheme:")
   (scale/improve-new-text
    (prog*
     "(if-exp (test-exp then-exp else-exp)"
     "  (let ((test-type (type-of-expr test-exp tenv))"
     "        (then-type (type-of-expr then-exp tenv))"
     "        (else-type (type-of-expr else-exp tenv)))"
     "    ;; succeeds or signals an error:"
     "    (check-equal-type! test-type bool-type)"
     "    (check-equal-type! then-type else-type)"
     "    then-type)")
    0.9))
	 
  (slide/title
   (i "proc-exp")
   (alg-code "proc(intTd x, boolTd y)if y then x else 0")
   (page-item "The rule from previous lecture:")
   (infer (alg-code "Env |- proc(Type_1 id_1, ... Type_n id_n)Expr : (*Type_1 *X* .... Type_n *-> Type_0*)")
	  (alg-code "{ id_1 : Type_1, .... id_n : Type_n } + Env |- Expr : Type_0"))
   (page-item "In Scheme:")
   (scale/improve-new-text
    (prog*
     "(proc-exp (texps ids body)"
     "  (let* ((arg-tys (expand-tyexprs texps))"
     "         (new-tenv (extend-tenv ids arg-tys tenv))"
     "         (res-type (type-of-expr body new-tenv)))"
     "    (proc-type arg-types res-type)))")
    0.9))
	 
  (slide/title
   (i "app-exp")
   (alg-code "(proc(intTd x, intTd y)+(x,y)  6  7)")
   (page-item "The rule from previous lecture:")
   (infer (alg-code "Env |- (Expr_0 Expr_1 .... Expr_n) : Type_0")
	  (ante-append/close
	   (alg-code "Env |- Expr_0 : (*Type_1 *X* .... Type_n *-> Type_0*)")
	   (alg-code "Env |- Expr_1 : Type_1")
	   (alg-code "....")
	   (alg-code "Env |- Expr_n : Type_n")))
   (page-item "In Scheme:")
   (prog*
    "(app-exp (rator rands)"
    "   (type-of-application"
    "     (type-of-expression rator tenv)"
    "     (types-of-expressions rands tenv)))"))

  (slide/title
   (i "app-exp")
   (scale/improve-new-text
    (prog*
     "(define (type-of-application rator-ty rand-tys)"
     "  (cases type rator-ty"
     "    (proc-type (arg-tys result-ty)"
     "      (if (= (length arg-tys) (length rand-tys))"
     "          (begin"
     "            (check-equal-types! rand-tys arg-tys)"
     "            result-ty)"
     "           (error 'wrong-arg-count)))"
     "    (else (error 'not-a-proc))))")
    0.9))
	 
  (slide/title
   (i "primapp-exp")
   (alg-code "+(1, 2)")
   (page-item "The rule from previous lecture:")
   (infer (alg-code "Env |- +(Expr_1, Expr_2) : numT")
	  (ante-append
	   (alg-code "Env |- Expr_1 : numT")
	   (alg-code "Env |- Expr_1 : numT")))
   (page-item "In Scheme (completely different):")
   (prog*
    "(primapp-exp (prim rands)"
    "  (type-of-application"
    "   (type-of-primitive prim)"
    "   (types-of-expressions rands tenv))"))

  (slide/title
   (i "primapp-exp")
   (prog*
    "(define (type-of-primitive prim)"
    "   (cases primitive prim"
    "     (add-prim ()"
    "        (proc-type (list int-type int-type)"
    "                   int-type))"
    "     ...))"))

  (slide/title
   (i "let-exp")
   (alg-code*
    "let x = 5"
    "      f = proc(intTd y)false"
    " in (f x)")
   (page-item "In Scheme:")
   (prog*
    "(let-exp (ids rands body)"
    "  (let* ((rand-tys (types-of-exprs rands tenv))"
    "         (body-tenv (extend-tenv ids rand-tys"
    "                                     tenv)))"
    "    (type-of-expression body body-tenv)))"))

  (define (mk-letrec first?)
    (scale/improve-new-text
     (apply
      prog*
      (append
       (list
	"(letrec-exp (res-texps proc-ids texpss idss bodies"
	"                body)")
       (if first?
	   (list
	    " (let*((arg-tyss (expand-tyexprss texpss))"
	    "       (res-tys (expand-tyexprs res-texps))"
	    "       (proc-tys (map proc-type arg-tyss res-tys)))"
	    "       (new-tenv (extend-tenv proc-ids proc-tys"
	    "                              tenv)))")
	   (list " ..."))
       (if first?
	   (list "   ...)")
	   (list
	    "   (for-each"
	    "     (lambda (ids arg-tys body res-ty)"
	    "        (check-equal-type! res-ty"
	    "           (type-of-expr body" 
	    "              (extend-tenv ids arg-tys new-tenv))))" 
	    "     idss arg-tyss bodies res-tys)"
	    "   (type-of-expression body new-tenv))"))))
     0.9))

  (slide/title
   (i "letrec-exp")
   (alg-code*
    "letrec int f(intTd x) = (g +(x,1) false)"
    "           int g(intTd y, boolTd b) = if b then (f y) else y"
    "  in (g 10 true)")
   (page-item "In Scheme:")
   'alts
   (list
    (list (mk-letrec #t))
    (list (mk-letrec #f))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Type-Checking Expressions"
   (page-item "What is the type of the following expression?")
   (blank)
   (alg-code "proc(x)+(x,1)")
   'next
   (blank)
   (page-item (colorize (bt "Answer: ") blue)
	      "Yet another trick question; it's not an expression"
	      "in our typed language, because the argument type is missing")
   'next
   (page-item "But it seems like the answer" (it "should") "be" (alg-code "(*intT *-> intT*)")))

  (slide/title
   "Type Inference"
   (page-item (dt "Type inference") "is the process of inserting type annotations"
	      "where the programmer omits them")
   (blank)
   (page-item "We'll use explicit question marks, to make it clear where types are"
	      "omitted")
   (blank)
   (alg-code "proc (? x)+(x,1)")
   'next
   (grammar-table
    (list
     (alg-code "tyexpr") eqls (alg-code "intTd") (blank)
     (blank)           -or- (alg-code "boolTd") (blank)
     (blank)           -or- (alg-code "(tyexpr _> tyexpr)") (blank)
     (blank)           -or- (alg-code "?") (blank))))
  
  (require "utils/ttree.ss")

  (define (sub a b)
    (hbl-append (text a `(bold . ,main-font) font-size)
		(colorize (text b `(bold subscript . ,main-font) font-size) BlueColor)))
  
  (define (implies a b)
    (hbl-append (* 2/3 font-size)
		a
		(text (string (integer->char 222)) 'symbol font-size)
		b))

  (define (-> a b)
    (hbl-append (alg-code "(*")
		a
		(alg-code " *-> ")
		b
		(alg-code "*)")))
  
  (define (intx-t-is-int i n)
    (let ([a (hb-append (sub "T" n)
			(colorize (t " = ") red)
			(alg-code "intT"))])
      (hbl-append font-size
		  (ghost a)
		  (alg-code i)
		  a)))

  (slide/title
   "Type Inference"
   'alts~
   (type-tree "proc(?_1 x)" "+(" "x" ", " "1" #f #f ")" 
	      (sub "T" "1")
	      "intT" #f (intx-t-is-int "intT" "1")
	      "(*intT *-> intT*)" vr-append)
   'next
   'alts
   (list
    (list (blank)
	  (page-item "Create a new type variable for each" (alg-code "?"))
	  (page-item "Change type comparison to install type equivalences"))
    (list (blank)))
   'alts~
   (type-tree "" "proc(?_1 x)if " "true" " then " "1" " else " "x" ""
	      "boolT" "intT" (sub "T" "1")
	      (intx-t-is-int "(*intT *-> intT*)" "1")
	      #f vr-append))

  (slide/title
   "Type Inference: Impossible Cases"
   'alts~
   (type-tree "" "proc(?_1 x)if " "x" " then " "1" " else " "x" ""
	      (sub "T" "1") "intT" (sub "T" "1")
	      (colorize
	       (hbl-append
		(colorize (bit "no type: ") red)
		(sub "T" "1")
		(t " can't be both ")
		(alg-code "boolT")
		(t " and ")
		(alg-code "intT"))
	       "black")
	      #f vr-append))

  (define poly
    (type-tree "" "proc(?_1 y)" "y" "" #f #f #f ""
	       (sub "T" "1") #f #f
	       (-> (sub "T" "1")
		   (sub "T" "1"))
	       #f
	       vr-append))

  (slide/title
   "Type Inference: Many Cases"
   'alts~
   poly
   'next
   (page-item "Sometimes, more than one type works")
   (page-subitem (alg-code "(*intT *-> intT*)"))
   (page-subitem (alg-code "(*boolT *-> boolT*)"))
   (page-subitem (alg-code "(*(*intT *-> boolT*) *-> (*intT *-> boolT*)*)"))
   (page-para (ghost bullet) "so the type checker leaves variables in the reported type"))

  (slide/title
   "Type Inference: Function Calls"
   'alts~
   (let* ([first-t1 (sub "T" "1")]
	  [i->i (alg-code "(*intT *-> intT*)")]
	  [stages
	   (type-tree "" "(" "proc(?_1 y)y" "  " "proc(?_2 x)+(x, 1)" #f #f ")"
		      (-> first-t1 (sub "T" "1")) i->i #f
		      (launder i->i)
		      #f vc-append)])
     (let loop ([l stages])
       (if (null? (cddr l))
	   (cons
	    (car l)
	    (map
	     (lambda (s)
	       (list
		(vr-append
		 line-sep
		 (cc-superimpose
		  (car s)
		  (linewidth
		   2
		   (colorize
		    (cons-picture
		     (ghost (car s))
		     (let-values ([(tl tb) (find-lb (car s) first-t1)]
				  [(il ib) (find-lb (car s) i->i)])
		       `((connect ,tl ,(+ tb 2)
				  ,(+ tl (pict-width first-t1)) ,(+ tb 2))
			 (connect ,il ,(+ ib 2)
				  ,(+ il (pict-width i->i)) ,(+ ib 2)))))
		    red)))
		 (hb-append (sub "T" "1")
			    (colorize (t " = ") red)
			    (launder i->i)))))
	     l))
	   (cons (car l) (loop (cdr l)))))))

  (slide/title
   "Type Inference: Function Calls"
   'alts~
   (type-tree "proc(?_1 y)" "(" "y" " " "7" #f #f ")"
	      (sub "T" "1") "intT" #f
	      (let ([a (hb-append (sub "T" "1")
				  (colorize (t " = ") red)
				  (->
				   (alg-code "intT")
				   (sub "T" "2")))])
		(hbl-append font-size
			    (ghost a)
			    (sub "T" "2")
			    a))
	      (-> (-> (alg-code "intT") (sub "T" "2"))
		  (sub "T" "2"))
	      vr-append)
   'next
   (blank)
   (page-item "In general, create a new type variable record for the result of a function call"))

  (define self-app (type-tree "proc(?_1 x)" "(" "x" " " "x" #f #f ")"
			      (sub "T" "1") (sub "T" "1") #f
			      (colorize
			       (hbl-append
				(colorize (bit "no type: ") red)
				(sub "T" "1")
				(t " can't be ")
				(->
				 (sub "T" "1")
				 (t "...")))
			       "black")
			      #f vr-append))

  (slide/title
   "Type Inference: Cyclic Equations"
   'alts~
   self-app
   'alts
   (list
    (list
     (page-item (sub "T" "1") "can't be" (alg-code "intT"))
     (page-item (sub "T" "1") "can't be" (alg-code "boolT"))
     (page-item "Suppose" (sub "T" "1") "is" (->
					      (sub "T" "2")
					      (sub "T" "3")))
     (page-subitem (sub "T" "2") "must be" (sub "T" "1"))
     (page-subitem "So we won't get anywhere!"))
    (list
     (blank)
     (page-item "When installing a type equivalence,"
		"make sure that the new type for" (alg-code "T")
		"doesn't already contain" (alg-code "T")))))
  
  (slide/title
   "Implementing Type Inference"
   (page-item "Extend" (tt "type") "datatype with" (tt "tvar-type") "variant")
   (prog*
    "(define-datatype"
    " type type?"
    " ..."
    " (tvar-type (serial-number integer?)"
    "            (container vector?)))")
   (page-item "Create a new type variable record for each" (alg-code "?"))
   (page-subitem "Initial container value is \"don't know\", " (alg-code "'()"))
   (page-item "Create a new type variable record for each application")
   (page-item "Change" (tt "check-equal-type!") "to read and set type variable containers"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
