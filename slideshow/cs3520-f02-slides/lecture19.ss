
(module lecture19 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/obj.ss"
	   "utils/ttree.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "More Optimization"
   (page-item "Still have list walks: variable lookup, method lookup")
   (page-subitem "Can eliminate many with lexical addresses")
   (page-subitem "Can eliminate some by pre-computing method positions")
   (page-subitem "Need type information to eliminate others"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (add-to-tree2 p)
    (slide/title
     "More Optimization: Eliminating List Walks"
     (scale (tree2 #f (colorize p purple)) 0.8)))

  (let ([p (alg-code "size=+(size,s)")])
    (add-to-tree2 p)
    (add-to-tree2 
     (vl-append line-sep
		p
		(colorize (alg-code "<1,0>=+(<1,0>,<0,2>)")
			  red))))

  (let ([p (vl-append
	    line-sep
	    (colorize (alg-code "In pickyfish:") "black")
	    (alg-code "super grow(-(f,1))"))])
    (add-to-tree2 p)
    (add-to-tree2 
     (vl-append line-sep
		p
		(colorize (alg-code "fish.grow(-(<0,2>,1))")
			  red))))
  (let ([p (vl-append
	    line-sep
	    (colorize (tt "In pickyfish:") "black")
	    (alg-code "send self grow(s)"))])
    (add-to-tree2 p)
    (add-to-tree2 
     (vl-append line-sep
		p
		(colorize (alg-code "send <1,0> <2>(<0,0>)")
			  red))))
  (let ([p (alg-code "send o grow(8)")])
    (add-to-tree2 p)
    (add-to-tree2 
     (vl-append line-sep
		p
		(colorize (it "need type of o!") red))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (new-c1-tree type)
    (type-tree "" "" "new c1()" #f #f #f #f "" 
	       type #f #f #f #f vc-append))
  
  (slide/title
   "Object Types"
   'alts~
   (append
    (new-c1-tree "objectT")
    (list
     (append (cadr (new-c1-tree "objectT[c1]"))
	     (list (page-item "Need to know class of object")))
     (append (cadr (new-c1-tree "c1T"))
	     (list (page-item "Class name as type implies object type")))
     (cadr (new-c1-tree "c1T"))))
   (page-para "..." (bit "if") (alg-code "c1") "has an" (alg-code "initialize")
	      "method that takes no arguments")
   (alg-code*
    "class c1 extends ..."
    " method void initialize() ..."))

  (slide/title
   "Object Types"
   'alts~
   (type-tree "" "new c1(" "5" ")" #f #f #f "" 
	      "intT" #f #f "c1T" #f vr-append)
   (page-para "... if" (alg-code "c1") "has an" (alg-code "initialize")
	      "method that takes one integer")
   (alg-code*
    "class c1 extends ..."
    " method void initialize(int v) ..."))

  (slide/title
   "Object Types"
   'alts~
   (type-tree "" "send " "new c1()" " m(" "false" ")" #f "" 
	      "c1T" "boolT" #f "intT" #f vc-append)
   (page-para "... if" (alg-code "c1") "has an" (alg-code "m")
	      "method that takes" (alg-code "boolT") "and returns" (alg-code "intT"))
   (alg-code*
    "class c1 extends ..."
    " method void initialize() ..."
    " method intTd m(boolTd v) ..."))

  (define fish-prog
    (alg-code*
     "class fish extends object"
     "  field int size"
     "  method void initialize (int s) ..."
     "  method void eat(fish other) ..."
     "class colorfish extends fish"
     "  ..."))
  
  (slide/title
   "Object Types"
   fish-prog
   'next
   (blank)
   'alts~
   (append
    (type-tree "" "send " "new fish(8)" " eat(" "new fish(1)" ")" #f "" 
	       "fishT" "fishT" #f "voidT" #f vc-append)
    (type-tree "" "send " "new fish(8)" " eat(" "new colorfish(1)" ")" #f "" 
	       "fishT" "colorfishT" #f 
	       (page-para* (alg-code "colorfishT") (colorize (bit "doesn't match") red)
			   (alg-code "fishT"))
	       #f vc-append)))

  (slide/title
   "Subtyping"
   (page-item (bit "Subtype:")
	      "An instance of class" (alg-code "C")
	      "can be used as an instance of class"
	      (alg-code "C'") "if" (alg-code "C") "is derived from" (alg-code "C'"))
   (alg-code "C <: C'")
   'next
   (blank)
   (page-item "Subtype rule:")
   (page-para* "If" (alg-code "Env |- Expr : Type_1") "and" (alg-code "Type_1 <: Type_2") ", then" (alg-code "Env |- Expr : Type_2"))
   (blank)
   (infer (alg-code "Env |- Expr : Type_2")
          (ante-append (alg-code "Env |- Expr : Type_1")
                       (alg-code "Type_1 <: Type_2"))))

  (slide/title
   "Object Types"
   fish-prog
   (blank)
   'alts~
   (type-tree "" "send " "new fish(8)" " eat(" "new colorfish(1)" ")" #f "" 
	      "fishT" 
              (alg-code "colorfishT <: fishT")
	      #f "voidT" #f vc-append))


  (slide/title
   "Language Changes"
   (page-item "Add types to field declarations")
   (page-item "Add types to method arguments and result")
   'next
   (blank)
   (page-item "Add" (alg-code "abstract class") "and" (alg-code "abstractmethod"))
   (page-item "Add" (alg-code "cast")))
  

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Program Checking"
   (ht-append
    (* 3 font-size)
    (tree-types #f #f)
    (let ([p (hbl-append
              (alg-code*
               "send"
               " new fish(3)"
               "get__size()")
              (colorize (alg-code ": intT") blue))])
      (inset p (- (* 0.8 (pict-width p))) 0 0 0))))

  (slide/title/tall
   "Things to Check"
   (page-para (alg-code "cast") ":")
   (page-item "Operand has an object type (for any class)")
   (page-item "Target class exists")
   (blank)
   'alts
   (list
    (list (alg-code "cast o c1"))
    (list
     (page-item "Class for operand and target must be comparable")
     (page-subitem "Otherwise, cast cannot possibly succeed")
     (alg-code*
      "class c1 extends object ..."
      "class c2 extends object ..."
      "cast new c1() c2"))))

  (slide/title/tall
   "Things to Check"
   (page-para "Object creation:")
   (page-item "Class exists, and is not abstract")
   (page-item "Class has an" (alg-code "initialize") "method")
   (page-item (alg-code "initialize") "'s argument types match the operand types")
   (blank)
   (alg-code*
    "class c1 extends object"
    "  method voidTd initialize(intTd x, boolTd y)"
    "   ...."
    " " 
    "new c1(1, false)"))

  (slide/title/tall
   "Things to Check"
   (page-para "Method calls:")
   (page-item "Receiver expression is an object")
   (page-item "Method is in the object-type's class")
   (page-subitem "Except" (alg-code "initialize") "...")
   (page-item "Method's argument types match the operand types")
   (blank)
   (alg-code*
    "class c1 extends object"
    "  method voidTd initialize() ..."
    "  method voidTd m(intTd x, boolTd y)"
    "   ...."
    "let o1 = new c1()"
    "in send o1 m(1, false)"))

  (slide/title/tall
   "Things to Check"
   (page-para (alg-code "super") "calls:")
   (page-item "Expression is within a method")
   (page-item "Method is in the superclass, and not abstract")
   (page-item "Method's argument types match the operand types")
   (blank)
   (alg-code*
    "class c1 extends object"
    "  method voidTd m(intTd x, boolTd y)"
    "    ...."
    "  " 
    "class c2 extends c1"
    "  method void n()"
    "    super m(1, false)"
    "  ...."))

  (slide/title/tall
   "Things to Check"
   (page-para (alg-code "class") "declarations:")
   (page-item "Superclass exists, and no cyclic inheritance")
   (page-item "Methods bodies ok")
   (page-subitem "Use host class for type of" (alg-code "self"))
   (page-item "Overriding method signatures are the same as in superclass")
   (page-subitem "Except for" (alg-code "initialize"))
   (alg-code*
    "class c2 extends c1"
    "  method intTd m(intTd x, boolTd y)"
    "   if y then +(2, x) else send self w()"))

  (define (mk-init ?)
    (slide/title/tall
     "The Initialize Method"
     (alg-code$* 
      (case ? 
        [(0)
         "method void initialize.*"]
        [(1)
         "send self initialize.."]
        [(2)
         "super initialize.*"])
      "class c1 extends obj"
      " field int x"
      " method void initialize()"
      "  set x = 3"
      " method int m()"
      "  send self initialize()"
      "  " 
      "class c2 extends c1"
      " field int y"
      " method void initialize(int v)"
      "   set y = v"
      "   super initialize()"
      " ....")
   (case ?
    [(0)
     (page-item "Derived class needs different signature for " (alg-code "initialize"))]
    [(1)
     (page-item "Disallow" (alg-code "send") "to" (alg-code "initialize"))]
    [(2)
     (page-item (alg-code "super") "call to" (alg-code "initialize") "is ok")])))
  
  (mk-init 0)
  (mk-init 1)
  (mk-init 2)

  (slide/title/tall
   "Field Initializations"
   (page-para (bit "Not") "checked: field initializations")
   (blank)
   (alg-code*
    "class interior__node extends tree"
    " field tree left"
    " field tree right"
    " method void initialize(tree l, tree r)"
    "  begin"
    "   send left sum();"
    "   ...."
    "  end")
   (blank)
   (page-item "Can get \"bad object 0 for method call\"")
   (page-item "This is analogous to the" (tt "null") "error in Java"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title/tall
   "Type Checking and Errors"
   (page-para (bt "Disallowed errors:"))
   (page-item "Object has no such method, or Super method not found")
   (page-item "Can't call method of non-object, non-0")
   (page-item "No such field, no such variable")
   (page-item "Illegal primitive argument (except car of empty)")
 
   (page-para (bt "Allowed errors:"))
   (page-item "Can't call method of 0")
   (page-item "Cast failed")
   (page-item "Car of empty"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (mk-basicfishprog pat feedarg)
    (alg-code$*
     pat
     (format "let feed = proc(~a f)" feedarg)
     "                  send f grow(10)"
     "    o1 = new colorfish(0)"
     " in "
     "   (feed o1)"))

  (slide/title
   "Mixing Subtyping and Procedures"
   (page-para "Our language still has procedures:")
   (page-para (mk-basicfishprog "!" "colorfish")))

  (define (mk-fishprog pat feedarg)
    (alg-code$*
     pat
     (format "let feed = proc(~a f)" feedarg)
     "                  send f grow(10)"
     "    o1 = new colorfish(0)"
     "    o2 = new colorfish(1)"
     " in let toboth = proc((colorfishTd _> voidTd) p)"
     "                             begin"
     "                                (p o1);"
     "                                (p o2)"
     "                             end"
     "     in (toboth feed)"))

  (slide/title
   "Mixing Subtyping and Procedures"
   (page-para "And higher-order procedures:")
   (page-para (mk-fishprog "!" "colorfish")))

  (slide/title/tall
   "Mixing Subtyping and Procedures"
   (page-para "Subtyping on procedure arguments:")
   'alts
   (list
    (list
     (page-para (mk-basicfishprog "fish f" "fish"))
     'next
     (page-item "This works, and is allowed by our subtyping rule"))
    (list
     (page-para (mk-fishprog "fish f" "fish"))
     'next
     (page-item "This works, but is" (bit "not")
                "allowed by our subtyping rule")
     (page-para* (alg-code "(*fishT *-> voidT*)")
                 " versus "
                 (alg-code "(*colorfishT *-> voidT*)")))))
	
  (define (venn spec gen)
    (define spec-color purple)
    (define gen-color orange)
    (hc-append
     font-size
     (cc-superimpose
      (colorize (disk (* 2.6 font-size)) gen-color)
      (inset (colorize (disk font-size) spec-color)
             (* 1/4 font-size) 0 0 (* 3/4 font-size)))
     (vl-append
      (/ font-size 2)
      (colorize (bt spec) spec-color)
      (colorize (bt gen) gen-color))))
  
  (slide/title/tall
   "Procedure Subtyping Rule"
   (page-para* "If" (alg-code "Type_1 <: Type_10") " and " (alg-code "Type_2 <: Type_20"))
   (page-para* "then" (alg-code " (*Type_10 *-> Type_2*) <: (*Type_1 *-> Type_20*)"))
   'next
   'alts
   (list
    (list
     (page-para "Another example:")
     (page-item (alg-code "dogT <: animalT"))
     (page-subitem "a dog can go anywhere an animal can go")
     (page-item (alg-code "(*animalT *-> hairstyleT*) <: (*dogT *-> hairstyleT*)"))
     (page-subitem "a groomer for all animals can"
                   "groom a dog")
     (page-subitem "a groomer who only works with dogs"
                   "doesn't work for all animals"))
    (list
     (page-para "General intuition:")
     (page-item (alg-code "Type_1 <: Type_10") "means" (alg-code "Type_10") 
                "is more general than" (alg-code "Type_1"))
     (venn "dog" "animal")
     (page-item "A function that is willing to accept a more general"
                "argument is itself more specific")
     (venn "(animal -> T)" "(dog -> T)"))
    (list
     (blank)
     (page-item "Procedure types are" (dt "contravariant")
                "with respect to their argument types")
     (page-item "Procedure types are" (dt "covariant")
                "with respect to their result types"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)
