
(module lecture16 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/ttree.ss"
	   (lib "list.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define (black p) (colorize p "black"))

  (slide/title
   "HW 7"
   (page-para "For HW 7, your task is to finish a type checker that"
	      "ensures only same-shaped trees are combined.")
   (page-item (alg-code "+(1,2)") "is ok")
   (page-item (alg-code "+(1,cons(1,2))") "must be rejected")
   (blank)
   (page-para "Is the resulting type system realistic (i.e., useful in practice)?")
   'next
   (blank)
   (colorize (page-para "No.") RedColor)
   (page-para "To see why, look at the" (alg-code "heavier-tree")
	      "function of exercise 5.3, since tree-processing functions are highly practical..."))

  (define (orig-5.3 hilite?)
    (slide/title
     "Program from Exercise 5.3"
     (alg-code$* 
      (if hilite? ".f cons.car.*f." "NONE")
      "proc(nt)"
      " let ht = proc(ntf)"
      "               let nt = car(ntf)"
      "                     f = cdr(ntf)"
      "               in"
      "                 if iscons(nt)"
      "                   then cons((f cons(car(nt), f)), "
      "                                     (f cons(cdr(nt), f)))"
      "                   else +(nt,1)"
      " in (ht cons(nt, ht))")
     ((if hilite? values ghost)
      (page-para "Assume" (alg-code "f")"'s type is" (alg-code "(*T_1 *-> T_2*)") "."))
     ((if hilite? values ghost)
      (colorize (page-para "Can't solve" (black (alg-code "T_1 = ([*T_3 *:* (*T_1 *-> T_2*)*])")) ".") RedColor))))

  (orig-5.3 #f)
  (orig-5.3 #t)
	  

  (define (letrec-5.3 hilite? problem?)
    (slide/title
     "Exercise 5.3 with letrec"
     (page-para "Does" (alg-code "letrec") "help?")
     (alg-code$* 
      (if hilite? ".ht car.nt.." "NONE")
      "proc(nt)"
      " letrec ht = proc(nt)"
      "                     if iscons(nt)"
      "                       then cons((ht car(nt)), "
      "                                         (ht cdr(nt)))"
      "                       else +(nt,1)"
      " in (ht nt)")
     ((if hilite? values ghost)
      (page-para "Assume" (alg-code "ht")"'s type is" (alg-code "(*([*T_1 *:* T_2*]) *-> T_3*)") "."))
     ((if hilite? values ghost)
      (colorize (page-para "Can't solve" (black (alg-code "([*T_1 *:* T_2*]) = T_1")) ".") RedColor))
     ((if problem? values ghost)
      (page-para "The problem is that" (alg-code "ht") "must work on many shapes of trees."))))

  (letrec-5.3 #f #f)
  (letrec-5.3 #t #f)
  (letrec-5.3 #t #t)

  (define (polymorphic-5.3 hilite?)
    (slide/title
     "Exercise 5.3 with letrec and Polymorphism"
     (page-para "Does polymorphism help?")
     (alg-code$* 
      (if hilite? "..nt,1." ".ht car.nt..")
      "proc(nt)"
      " letrec ht = proc(nt)"
      "                     if iscons(nt)"
      "                       then cons((ht car(nt)), "
      "                                         (ht cdr(nt)))"
      "                       else +(nt,1)"
      " in (ht nt)")
     (if hilite? 'nothing 'next)
     (page-para "Assume" (alg-code "ht")"'s type is" (alg-code "(*([*T_1 *:* T_2*]) *-> T_3*)") ".")
     (page-para "Need to solve" (alg-code "([*T_11 *:* T_12*]) = T_1") (colorize (it "  could be ok!") RedColor))
     ((if hilite? values ghost)
      (colorize (page-para "But can't solve" (black (alg-code "([*T_1 *:* T_2*]) = intT")) ".") RedColor))))

  (polymorphic-5.3 #f)
  (polymorphic-5.3 #t)

  (define (works-5.3 hilite?)
    (slide/title
     "Why Exercise 5.3 Works"
     (page-para "How can both we take the" (alg-code "car") "of" (alg-code "nt") "and add 1 to" (alg-code "nt") "?")
     (alg-code$* 
      (if hilite? "iscons.nt." "(.ht car.nt..)|(..nt,1.)")
      "proc(nt)"
      " letrec ht = proc(nt)"
      "                     if iscons(nt)"
      "                       then cons((ht car(nt)), "
      "                                         (ht cdr(nt)))"
      "                       else +(nt,1)"
      " in (ht nt)")
     ((if hilite? values ghost)
      (colorize (page-para "Because we test, first.") BlueColor))
     (if hilite?
	 'next
	 'nothing)
     ((if hilite? values ghost)
      (page-para "We need to tell the type system about this test."))))

  (works-5.3 #f)
  (works-5.3 #t)

  (define rest-tree-prog
    (alg-code* 
     "proc(nt)"
     " letrec ht = proc(nt)"
     "                 cases nt of"
     "                   ([*treeT *:* treeT*]): cons((ht car(nt)), "
     "                                                      (ht cdr(nt)))"
     "                   intT: +(nt,1)"
     " in (ht nt)"))

  (slide/title
   "Telling a Type System About Datatypes"
   (page-para "Forms like" (alg-code "define-datatype") "and" (alg-code "cases")
	      "are exactly what we need to tell the type system about variants and tests.")
   (lc-superimpose
    (alg-code* 
     "treeT = ([* treeT *:* treeT *])  or  intT")
    (blank (pict-width rest-tree-prog) 1))
   'alts
   (list (list
	  rest-tree-prog)
	 (list
	  (page-para (colorize (t "Yet another technical problem:") RedColor))
	  (page-item "Is the type of" (alg-code "5") (alg-code "intT") "or" (alg-code "treeT") "?")
	  (page-item "Is the type of" (alg-code "cons(5,5)") (alg-code "([*intT *:* intT*])")
		     "or " (alg-code "([*treeT *:* intT*])")
		     "or " (alg-code "([*treeT *:* treeT*])")
		     "or" (alg-code "treeT")
		     "?")
	  'next
	  (colorize (page-para "Solution: use explicit constructors.") BlueColor))))

  (slide/title
   "Datatype Constructors"
    (alg-code* 
     "treeT = (Branch ([* treeT *:* treeT *]))  or  (Leaf intT)"
     " "
     "let grow ="
     "  proc(nt)"
     "   letrec ht = proc(nt)"
     "                       cases treeT nt of"
     "                         [(Branch p) (Branch cons((ht car(p)), "
     "                                                                      (ht cdr(p)))]"
     "                         [(Leaf n) (Leaf +(n,1))]"
     "   in (ht nt)"
     "in (grow (Branch cons((Leaf 1), (Leaf 2))))")
    'next
    (page-para "With a more flexible datatype type declaration, we can simplify this"
	       "and get rid of the" 
	       (alg-code "cons") "primitive completely."))
    

  (slide/title
   "Datatype Constructors without cons"
    (alg-code* 
     "treeT = (Branch treeT treeT)  or  (Leaf intT)"
     " "
     "let grow ="
     "  proc(nt)"
     "   letrec ht = proc(nt)"
     "                       cases treeT nt of"
     "                         [(Branch l r) (Branch (ht l) (ht r))]"
     "                         [(Leaf n) (Leaf +(n,1))]"
     "   in (ht nt)"
     "in (grow (Branch (Leaf 1) (Leaf 2))"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Using Datatypes Types"
   (page-item "More examples in DrScheme")
   (page-item "HW 8: implement an interpreter using this book language")
   (page-subitem "A type-checker and interpreter for this book language is provided")
   (page-subitem "You write" (it "another") "interpreter layer on top of the book language interpreter")
   (vc-append (frame (inset (t "HW 8 interpreter") 2))
	      (cb-superimpose
	       (vline 1 font-size)
	       (arrowhead (/ font-size 2) (- (/ pi 2))))
	      (frame (inset (t "latest book-language interpreter") 2))
	      (cb-superimpose
	       (vline 1 font-size)
	       (arrowhead (/ font-size 2) (- (/ pi 2))))
	      (frame (inset (t "DrScheme interpreter") 2))))

  (slide/title
   "HW 8 Setup"
   (scale/improve-new-text
    (prog*
     "exp = (var num) or (lit num) or (lam num exp)"
     "   or (app exp exp) or (plus exp exp)"
     "   or (minus exp exp) or (ifz exp exp exp)"
     "in let sum10 = "
     " (app (lam 19 (app (app (var 19) (var 19))"
     "                   (lit 10)))"
     "      (lam 18"
     "       (lam 17 "
     "         (ifz (var 17) "
     "              (lit 0)"
     "              (plus (var 17)"
     "                    (app (app (var 18) (var 18))"
     "                         (minus (var 17) (lit 1))))))))"
     "in letrec ... eval ... = ..."
     "in (eval sum10 ...)")
    0.9))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Elements of a Realistic Type System"
   (page-item "A few primitive types")
   (page-item "Datatype declarations and dispatching")
   (blank)
   (page-item "Inference")
   (page-item "Polymorphism")
   (page-item "Datatype Polymorphism")
   (colorize (page-para "The last three are not necessary for HW 8.") BlueColor))

  (slide/title
   "Toward Datatype Polymorphism"
   (scale/improve-new-text
    (prog*
     " "
     "numlst = (empty) or (cons num numlst)"
     "in"
     "letrec numlst map((num -> num) f, numlst l)"
     "           = cases numlst l of"
     "              [(empty) (empty)]"
     "              [(cons i l) (cons (f i) (map f l))]"
     "in let l = (cons 1 (cons 20 (cons 14 (empty))))"
     "in (map proc(num x)+(x,1) l)")
    0.9))

  (slide/title
   "Toward Datatype Polymorphism"
   (scale/improve-new-text
    (prog$*
     "(numlstlst = .*)|(lcons)|(lempty)|numlstlst|(.num -> numlst.)|(proc.*empty..)"
     "numlstlst = (lempty) or (lcons numlst numlstlst)"
     "numlst = (empty) or (cons num numlst)"
     "in"
     "letrec numlstlst map((num -> numlst) f, numlst l) "
     "           = cases numlst l of"
     "              [(empty) (lempty)]"
     "              [(cons i l) (lcons (f i) (map f l))]"
     "in let l = (cons 1 (cons 20 (cons 14 (empty))))"
     "in (map proc(num x)(cons x (empty)) l)")
    0.9)
   'next
   (blank)
   (colorize
    (page-para "Can we avoid multiple lst datatypes and multiple" (hbl-append (prog "map") (t "s?")))
    RedColor))

  (slide/title
   "Datatype Polymorphism"
   (scale/improve-new-text
    (prog*
     "<'a>lst = (empty) or (cons 'a <'a>lst)"
     "twolsts = (together <num>lst <<num>lst>lst)"
     "in"
     "letrec <'b>lst map(('a -> 'b) f, <'a>lst l) = "
     "              cases <'a>lst l of"
     "               [(empty) (empty)]"
     "               [(cons i l) (cons (f i) (map f l))]"
     "in let l = (cons 1 (cons 20 (cons 14 (empty))))"
     "   in (together (map proc(num x)+(x,1) l)"
     "                (map proc(num x)(cons x (empty)) l))")
    0.9)
   (page-item "Use" (hbl-append (prog "'") (alg-code "id")) "as a type variable")
   (page-item "Multiple uses of" (hbl-append (prog "'") (alg-code "id")) "in one type definition or"
	      "function signature correspond to the same type variable")
   ;; (page-item "Type variables subsume uses of" (alg-code "?") "for inference")
   )

  (slide/title
   "Languages with Datatype Polymorphism"
   (page-item "The checker and interpreter in" (tt "lecture16.scm") "implements the core of" (dt "ML"))
   (tt "http://caml.inria.fr/")
   (page-item "Java will eventually get a similar kind of data polymorphism"))

  (slide/title
   "Alternate HW 8"
   (page-para "If you prefer, you can implement HW 8 using OCaml.")
   (vc-append (frame (inset (t "HW 8 interpreter") 2))
	      (cb-superimpose
	       (vline 1 font-size)
	       (arrowhead (/ font-size 2) (- (/ pi 2))))
	      (frame (inset (t "OCaml interpreter") 2))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)

