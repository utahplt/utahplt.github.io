
(module lecture0 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss")

  (slide/center
   (vc-append
    line-sep
    (titlet "CS3520")
    (titlet "Programming Language Concepts"))
   (blank)
   (hbl-append
    (t "Instructor: ")
    (colorize (bt "Matthew Flatt") BlueColor)))

  (slide/title
   "Programming Language Concepts"
   (page-para "This course teaches concepts in two ways:")
   'next
   (page-item (colorize (t "By implementing interpreters") BlueColor))
   (page-subitem "new concept => extend interpreter")
   'next
   (page-item (colorize (t "By using") BlueColor) (colorize (bt "Scheme") BlueColor))
   (page-subitem "we assume that you" (it "don't") "already know Scheme"))

  (slide/title/center
   "Course Details"
   (tt "http://www.cs.utah.edu/classes/cs3520/"))

  (slide/title
   "Bootstrapping Problem"
   (page-item "We'll learn about languages by writing interpreters in Scheme")
   'next
   (page-item "We'll learn about Scheme...")
   'next
   (page-para (ghost (page-item* "")) "by writing an interpreter...")
   'next
   'alts~
   (list (list (page-para (ghost (page-item* "")) "in Scheme" (colorize (t "?") RedColor)))
	 (list (page-para (ghost (page-item* "")) "in" (strike RedColor (t "Scheme")) (colorize (bt "set theory") BlueColor))))
   'next
   (blank)
   (page-item "More specifically, we'll define Scheme as an extension of" (colorize (bt "algebra") BlueColor))
   'next
   (page-para* (it "Algebra is a programming language?")))

  (slide/title
   "Algebra as a Programming Language"
   (page-item "Algebra has a grammar:")
   (page-subitem (alg-code "(1 + 2)") "is a legal expression")
   (page-subitem (alg-code "(1 + +)") "is not a legal expression")
   'next
   (blank)
   (page-item "Algebra has rules for evaluation:")
   (page-subitem (alg-code "(1 + 2) == 3"))
   (page-subitem (alg-code "f(17) == (17 + 3) == 20")
		 "   if   "
		 (alg-code "f(x) = (x + 3)")))

  (define alg-slide (most-recent-slide))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "A Grammar for Algebra Programs"
   (page-para "The grammar in" (dt "BNF") "(Backus-Naur Form;" (it "EoPL") "sec 1.1.2):")
   complete-table
   'next
   (page-item "Each" (dt "meta-variable") ", such as" (alg-code "prog") ", defines a set"))

  (define (make-expr-case select examples done)
    (list* (grammar-table (expr-lines (or select -1)))
	   (if (not select)
	       (list (page-item "The set" (alg-code "expr") "is defined in terms of other sets"))
	       (list*
		(page-item "To make an example" (alg-code "expr") ":")
		(page-subitem "choose one case in the grammar")
		(page-subitem "pick an example for each meta-variable")
		(append
		 examples
		 (list (page-subitem "combine the examples with literal text")
		       done))))))

  (define 7-in-n (alg-code "7 isin num"))
  (define 7-in-expr (alg-code "7 isin expr"))
  (define f-in-id (alg-code "f isin id"))
  (define (mk-f-id-combo p)
    (hbl-append f-in-id (blank (* 4 font-size) 1) p))
  (define x-in-id (alg-code "x isin id"))
  (define f-in-id/7-in-expr (mk-f-id-combo 7-in-expr))
  (define f7-in-expr (alg-code "f(7) isin expr"))
  (define f-in-id/f7-in-expr (mk-f-id-combo f7-in-expr))
  (define ff7-in-expr (alg-code "f(f(7)) isin expr"))

  (define f-of-x (alg-code "f(x) = (x + 1)"))
  (define g-of-y (alg-code "g(y) = f((y - 2))"))
  (define g7 (alg-code "g(7)"))
  (define ex-in-prog
    (hc-append (vl-append
		line-sep
		f-of-x
		g-of-y
		g7)
	       (alg-code "   isin prog")))

  (define f-of-x-in-defn (hbl-append f-of-x (alg-code " isin defn")))
  (define g-of-y-in-defn (hbl-append g-of-y (alg-code " isin defn")))
  (define g7-in-expr (hbl-append g7 (alg-code " isin expr")))

  (define x+1-in-expr (alg-code "(x + 1) isin expr"))


  (slide/title
   "Using a BNF Grammar"
   (grammar-table id+n-lines)
   (page-item "The set" (alg-code "id") "is the set of all variable names")
   (page-item "The set" (alg-code "num") "is the set of all numbers")
   'next
   (blank)
   (page-item "To make an example member of" (alg-code "num") ", simply pick an element from the set")
   'next
   (page-para* (alg-code "1 isin num"))
   (page-para* (alg-code "198 isin num")))

  (slide/title
   "Using a BNF Grammar"
   'alts
   (list (make-expr-case #f #f #f)
	 (make-expr-case -1 null (blank))))
  
  (slide/title
   "Using a BNF Grammar"
   'alts~
   (list (make-expr-case 4 null (blank))
	 (make-expr-case 4 (list 7-in-n) (blank))
	 (make-expr-case 4 (list 7-in-n) 7-in-expr)))

  (slide/title
   "Using a BNF Grammar"
   'alts~
   (list (make-expr-case 2 null (blank))
	 (make-expr-case 2 (list f-in-id) (blank))
	 (make-expr-case 2 (list f-in-id/7-in-expr) (blank))
	 (make-expr-case 2 (list f-in-id/7-in-expr) f7-in-expr)))

  (slide/title
   "Using a BNF Grammar"
   'alts~
   (list (make-expr-case 2 (list f-in-id/f7-in-expr) (blank))
	 (make-expr-case 2 (list f-in-id/f7-in-expr) ff7-in-expr)))

  (slide/title
   "Using a BNF Grammar"
   (grammar-table prog+defn-lines)
   (page-para* (alg-code "f(x) = (x + 1) isin defn"))
   (blank)
   'next
   (page-item "To make a" (alg-code "prog") "pick some number of" (hbl-append (alg-code "defn") (t "s")))
   (alg-code "(x + y) isin prog")
   (blank)
   ex-in-prog)

  (slide/title
   "Demonstrating Set Membership"
   (page-item "We can run the element-generation process in reverse to"
	      (dt "prove")
	      "that some item is a member of a set")
   'next
   (page-item "Such proofs have a standard tree format:")
   (infer (it "claim to prove")
	  (ante-append
	   (it "sub-claim to prove")
	   (it "...")
	   (it "sub-claim to prove")))
   'next
   'alts
   (list (list (page-item "Immediate membership claims serve as leaves on the tree:")
	       'alts
	       (list (list (alg-code "7 isin num"))
		     (list (alg-code "f isin id"))))
	 (list (page-item "Other membership claims generate branches in the tree:")
	       'alts
	       (list
		(list (infer (alg-code "7 isin expr")
			     (alg-code "7 isin num")))
		(list (infer (alg-code "f(7) isin expr")
			     (ante-append
			      (alg-code "f isin id")
			      (infer (alg-code "7 isin expr")
				     (alg-code "7 isin num"))))))))
   'next
   (background YellowColor
	       (page-para* (it "The proof tree's shape is driven entirely by the grammar"))))

  (define (hilite-new l)
    (background PinkColor l))

  (slide/title
   "Demonstrating Set Membership: Example"
   (hilite-new (alg-code "f(7) isin expr"))
   'next
   (grammar-table (expr-lines 2))
   (blank)
   (page-item "Two meta-variables on the left means two sub-trees:")
   (page-subitem "One for" (alg-code "f isin id"))
   (page-subitem "One for" (alg-code "7 isin expr")))
  
  (slide/title
   "Demonstrating Set Membership: Example"
   (infer (alg-code "f(7) isin expr")
	  (ante-append
	   (hilite-new (alg-code "f isin id"))
	   (hilite-new (alg-code "7 isin expr"))))
   'next
   (grammar-table id-line)
   (grammar-table (expr-lines 4))
   (page-item (alg-code "f isin id") "is immediate")
   (page-item (alg-code "7 isin expr") "has one meta-variable, so one subtree"))

  (slide/title
   "Demonstrating Set Membership: Example"
   (infer (alg-code "f(7) isin expr")
	  (ante-append
	   (alg-code "f isin id")
	   (infer (alg-code "7 isin expr")
		  (hilite-new (alg-code "7 isin num")))))
   'next
   (grammar-table num-line)
   (page-item (alg-code "7 isin num") "is immediate, so the proof is complete"))
   
  (slide/title
   "Demonstrating Set Membership: Another Example"
   (hilite-new ex-in-prog)
   'next
   (grammar-table prog-line)
   'next
   (blank)
   (page-item "Three meta-variables (after expanding" (alg-code "**") ") means three sub-trees:")
   (page-subitem "One for" f-of-x-in-defn)
   (page-subitem "One for" g-of-y-in-defn)
   (page-subitem "One for" g7-in-expr))

  (slide/title
   "Demonstrating Set Membership: Example 2"
   (infer ex-in-prog 
	  (inset (hilite-new g-of-y-in-defn)
		 (- (pict-width f-of-x-in-defn)
		    (pict-width g7-in-expr))
		 0 0 0)
	  (ante-append (hilite-new f-of-x-in-defn)
		       (blank) (blank)
		       (hilite-new g7-in-expr)))
   'next
   (page-item "Each sub-tree can be proved separately")
   (page-item "We'll prove only the first sub-tree for now"))

  (slide/title
   "Demonstrating Set Membership: Example 2"
   (hilite-new f-of-x-in-defn)
   'next
   (grammar-table defn-line)
   'next
   (blank)
   (page-item "Three meta-variables, three sub-trees"))

  (slide/title
   "Demonstrating Set Membership: Example 2"
   (infer f-of-x-in-defn
	  (ante-append (hilite-new f-in-id)
		       (hilite-new x-in-id)
		       (hilite-new x+1-in-expr)))
   (blank)
   (page-item "The first two are immediate, the last requires work:")
   (grammar-table (expr-lines 0)))

  (slide/title
   "Demonstrating Set Membership: Example 2"
   (page-para "Final tree:")
   (infer f-of-x-in-defn
	  (ante-append f-in-id
		       x-in-id
		       (infer x+1-in-expr
			      (ante-append
			       (infer (alg-code "x isin expr")
				      x-in-id)
			       (infer
				(alg-code "1 isin expr")
				(alg-code "1 isin num"))))))
   (blank)
   (page-item "This was just one of three sub-trees for the original" 
	      (alg-code "isin prog")
	      "proof..."))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (re-slide alg-slide)

  (define 7-4-example
    (two-line-reduction
     (alg-code "f(x) = (x + 1)")
     (alg-code "(2 + (7 - 4))")
     (alg-code "(2 + 3)")))

  (define 7-4-example-cotd
    (two-line-reduction
     (alg-code "f(x) = (x + 1)")
     (alg-code "(2 + 3)")
     (alg-code "5")))

  (define (make-f-x-example extras)
    (two-line-reduction
     (apply vl-append
	    line-sep
	    (alg-code "f(x) = (x + 1)")
	    extras)
     (alg-code "(2 + f(13))")
     (alg-code "(2 + (13 + 1))")))

  (define f-x-example (make-f-x-example null))

  (define beta-reduction
    (vr-append
     (/ font-size 2)
     (two-line-reduction
      (alg-code "... id_1(id_2) = expr_1 ...")
      (alg-code "... id_1(expr_2) ...")
      (alg-code "... expr_3 ..."))
     (page-para* "where" (alg-code "expr_3") "is" (alg-code "expr_1") 
		 "with" (alg-code "id_2") "replaced by" (alg-code "expr_2"))))

  (slide/title
   "Evaluation Function"
   (page-item "An" (dt "evaluation function") "," (alg-code "->") ", takes a single evaluation step")
   (page-item "It maps programs to programs:")
   'alts
   (list
    (list (two-line-reduction
	   (blank)
	   (alg-code "(2 + (7 - 4))") 
	   (alg-code "(2 + 3)")))
    (list 7-4-example)
    (list (make-f-x-example (list
			     (alg-code "g(y) = (y - 1)")
			     (alg-code "h(z) = f(z)"))))))
  
  (slide/title
   "Evaluation Function"
   (page-item "Apply" (alg-code "->") "repeatedly to obtain a result:")
   7-4-example
   7-4-example-cotd)

  (slide/title
   "Evaluation Function"
   (page-item "The" (alg-code "->") "function is defined by a set of pattern-matching rules:")
   (blank)
   'next
   'alts
   (list (list
	  7-4-example
	  (t "due to the pattern rule")
	  (alg-code "... (7 - 4) ...  ->  ... 3 ..."))
	 (list
	  f-x-example
	  (t "due to the pattern rule")
	  beta-reduction)))

  (slide/title
   "Pattern-Matching Rules for Evaluation"
   (page-item (bt "Rule 1"))
   beta-reduction
   'next
   (page-item (bt "Rules 2 -") sym:infinity)
   (table 2
	  (list (alg-code "... (0 + 0) ...  ->  ... 0 ...") (alg-code "... (0 - 0) ...  ->  ... 0 ...")
		(alg-code "... (1 + 0) ...  ->  ... 1 ...") (alg-code "... (1 - 0) ...  ->  ... 1 ...")
		(alg-code "... (0 + 1) ...  ->  ... 1 ...") (alg-code "... (0 - 1) ...  ->  ... -1 ...")
		(alg-code "... (2 + 0) ...  ->  ... 2 ...") (alg-code "... (2 - 0) ...  ->  ... 2 ...")
		(it "etc.")               (it "etc."))
	  cc-superimpose cc-superimpose
	  (* 4 font-size) line-sep))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Homework"
   (page-item "Some evaluations")
   (page-item "Some membership proofs")
   (page-item "See the web page for details")
   (page-item "Due next Tuesday, August 27, 11:59 PM"))

  (slide/title
   "Where is This Going?"
   (page-para "Next time:")
   (page-item "Shift syntax slightly to match that of Scheme")
   (page-item "Add new clauses to the expression grammar")
   (page-item "Add new evaluation rules")
   (page-para "Current goal is to learn Scheme, but we'll use algebraic techniques all semester"))
  
  'done)
