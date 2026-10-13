(module lecture20 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   (lib "class.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define grades '(73
		   79
		   59
		   70
		   76
		   72
		   90
		   81
		   83
		   71
		   83
		   74
		   78
		   79
		   60
		   94
		   75
		   84
		   46
		   81
		   84
		   76
		   75
		   73
		   90
		   90
		   86
		   88
		   81
		   91
		   97
		   99
		   87
		   97
		   97
		   98
		   94
		   90
		   84
		   100
		   100
		   100
		   100))
  (define hi (apply max grades))
  (define lo (apply min grades))
  (define buckets (make-vector (- hi lo -1)))
  (for-each (lambda (i) 
	      (let ([i (- 100 i)])
		(vector-set! buckets i (add1 (vector-ref buckets i)))))
	    grades)
  (define plot (let ([w (* client-w 3/4)]
		     [h (* client-h 1/4)])
		 (dc (lambda (dc x y)
		       (let ([full (apply max (vector->list buckets))]
			     [len (vector-length buckets)])
			 (let loop ([i 0])
			   (unless (= i len)
			     (let ([bh (* (/ h full) (- full (vector-ref buckets i)))]
				   [bw (/ w len)])
			     (send dc draw-rectangle 
				   (+ x (* (/ w len) i))
				   (+ y bh)
				   bw (- h bh)))
			     (loop (add1 i))))))
		     w h 0 0)))
		     

  (slide/title/center
   "Mid-Term 2 Grades"
   (vc-append
    plot
    (hc-append
     (- (pict-width plot) (* 2 font-size))
     (t (format "~a" hi))
     (t (format "~a" lo)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "HW 9"
   (page-para "Homework 9, in untyped class interpreter:")
   (page-item "Add" (tt "instanceof"))
   (page-item "Restrict field access to local class")
   (page-item "Implement overloading (based on argument count)")
   (blank)
   (colorize (page-para* "Due date is the same as for HW 10") RedColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Implementing Type Checking with Classes"
   (page-para "We used to have two records for each class:")
   (page-item (it "Class declarations") "= abstract syntax")
   (vl-append
    line-sep
    (page-item (it "Class") "= run-time class information")
    (page-subitem "flattened field and method lists"))
   'next
   (page-para "Now we'll have three:")
   (page-item (it "Class declarations") "= abstract syntax")
   (vl-append
    line-sep
    (page-item (colorize (it "Static class") PurpleColor) "= check-time class information")
    (page-subitem "flattened lists with types"))
   (vl-append
    line-sep
    (page-item (it "Class") "= run-time class information")
    (page-subitem "flattened lists")))

  (slide/title
   "Static Class Elaboration"
   (scale/improve-new-text
    (prog$*
     "statically-elaborate-class-decls!"
     ";; type-of-program : program -> type"
     "(define (type-of-program pgm)"
     " (cases program pgm"
     "   (a-program (c-decls exp)"
     "     (statically-elaborate-class-decls! c-decls)"
     "     (type-of-expression exp (empty-tenv))))))")
    0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "Checking Class Declarations"
   (page-para "Check:")
   (page-item "Superclass exists, and no cyclic inheritance")
   (page-item "Methods bodies ok")
   (page-subitem "Use host class for type of" (alg-code "self"))
   (page-item "Overriding method signatures are the same as in superclass")
   (page-subitem "Except for" (alg-code "initialize"))
   (alg-code*
    "class c2 extends c1"
    "  method voidTd m(intTd x, boolTd y)"
    "   if y then +(2, x) else send self w()"))

  (slide/title/tall
   "Checking Class Declarations"
   (page-item "Cyclic inheritance covered by requirement that classes are ordered")
   (prog$*
    "check-class-method-bodies!"
    "(define statically-elaborate-class-decls!"
    "  (lambda (c-decls)"
    "    (for-each statically-elaborate-class-decl!"
    "              c-decls)"
    "    (for-each check-class-method-bodies!"
    "              c-decls)))"))
  
  (slide/title/tall
   "Checking Class Declarations: Methods"
   (prog$*
    "typecheck-method-decl!"
    "(define (check-class-method-bodies! c-decl)"
    "   ..."
    "   (for-each"
    "     (lambda (m-decl)"
    "       (typecheck-method-decl! "
    "         m-decl"
    "         class-name super-name"
    "         field-ids field-tys))"
    "     m-decls))"))

  (slide/title/tall
   "Checking Class Declarations: Methods"
   (scale/improve-new-text
    (prog*
     "(define (typecheck-method-decl! m-decl self-name"
     "                super-name field-ids field-types)"
     " (cases method-decl m-decl"
     "  (a-methd-decl (res-texp name id-texps ids body)"
     "   (let* ((id-tys (expand-ty-exprs id-texps))"
     "          (tenv"
     "            (extend-tenv"
     "             (cons '%super (cons 'self ids))"
     "             (cons (class-type super-name)"
     "                   (cons (class-type self-name)"
     "                         id-tys))"
     "             (extend-tenv"
     "              field-ids field-tys (empty-tenv))))"
     "           (body-ty (type-of-expr body tenv)))"
     "     (check-is-subtype!"
     "      body-ty (expand-ty-expr res-texp) m-decl)))"
     "  (an-abstract-method-decl (...) #t)))")
    0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "Checking Object Creation"
   (page-para "Check:")
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
  
  (slide/title
   "Checking Object Creation"
   (scale/improve-new-text
    (prog*
     "(define (type-of-new-obj-exp rand-types)"
     " (cases static-class (static-lookup class-name)"
     "  (a-static-class (...)"
     "    (cases abstraction-specifier specifier"
     "      (abstract-specifier ()"
     "        (eopl:error ...))"
     "      (concrete-specifier ()"
     "        (type-of-method-app-exp"
     "         #t ;; means from `new'"
     "         (class-type class-name) "
     "         'initialize"
     "         rand-types)"
     "        ;; Result:"
     "        (class-type class-name))))))")
    0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "Checking Method Calls"
   (page-para "Check:")
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
   "Checking Method Calls"
   (scale/improve-new-text
    (prog$*
     "type-of-method-app-or-super-call"
     "(define (type-of-method-app-exp for-new? obj-type"
     "                                msg rand-types)"
     "  (if (and (eq? msg 'initialize) (not for-new?))"
     "    (eopl:error ...))"
     "  (cases type obj-type"
     "    (class-type (class-name)"
     "      (type-of-method-app-or-super-call"
     "        #f class-name msg rand-types))"
     "    (else"
     "      (eopl:error ...))))")
    0.9))

  (slide/title/tall
   "Checking Super Calls"
   (page-para "Check:")
   (page-para "Same as method calls, but simpler:")
   (page-item "No check for" (alg-code "initialize"))
   (page-item "No possibility of a non-object type")
   (blank)
   (scale/improve-new-text
    (prog$*
     "type-of-method-app-or-super-call"
     "(define (type-of-super-call-exp super-name"
     "                                msg rand-types)"
     "    (type-of-method-app-or-super-call"
     "      #t super-name msg rand-types)")
    0.9))

    (slide/title/tall
     "Checking Method Application"
     (scale/improve-new-text
      (prog*
       "(define (type-of-method-app-or-super-call"
       "             super-call? host-name msg rand-tys)"
       " (let ((method (statically-lookup-method msg"
       "                (static-class->methods"
       "                  (static-lookup host-name)))))"
       "   (if (static-method? method)"
       "     (cases static-method method"
       "       (a-static-method (method-name spec"
       "                         method-ty super-name)"
       "         (let ((result-ty (type-of-app"
       "                            method-ty rand-tys)))"
       "           (if super-call?"
       "               (cases abstraction-specifier spec"
       "                 (concrete-spec () result-ty)"
       "                 (abstract-spec () (error ...)))"
       "               result-ty))))"
       "     (eopl:error ...))))")
      0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;

  (slide/title/tall
   "Checking Casts"
   (page-para "Check:")
   (page-item "Operand has an object type (for any class)")
   (page-item "Target class exists")
   (blank)
   (page-item "Class for operand and target must be comparable")
   (page-subitem "Otherwise, cast cannot possibly succeed")
   (alg-code*
    "class c1 extends object ..."
    "class c2 extends object ..."
    "cast new c1() c2"))
  
  (slide/title
   "Checking Casts"
   (scale/improve-new-text
    (prog*
     "(define (type-of-cast-exp ty name2 exp)"
     " (cases type ty"
     "  (class-type (name1)"
     "    (if (or (statically-is-subclass? name1 name2)"
     "            (statically-is-subclass? name2 name1))"
     "        (class-type name2)"
     "        (eopl:error ...)))"
     "  (else"
     "    (eopl:error ...)))))")
    0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Checking Other Expressions"
   (page-item "Other expression forms checked as before")
   (page-item (tt "check-is-subtype!") "often used instead of" (tt "check-equal-type!")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Compiling with Classes (Optionally)"
   (page-item "Recall that a" (dt "compiler") "takes a program in language"
              (it "A") "and produces a program in language" (it "B"))
   'next
   (page-item "To make compilation optional, a common trick is to set"
              (hbl-append (it "B") (t " = ") (it "A") (t ",")) "with the expectation that"
              "source programs use only a subset of" (it "A")))

  (slide/title
   "Grammar with Compiler-target Cases"
   (grammar-table
    (list
     (alg-code "expr")  eqls (alg-code "num") (blank)
     (blank)            -or- (alg-code "id") (blank)
     (blank)            -or- (alg-code "prim(expr**,)") (blank)
     (blank) (alg-code "....") (blank) (blank)
     (blank)            -or- (alg-code "send expr id(expr**,)") (blank)
     (blank) (alg-code "....") (blank) (blank)
     (blank)            -or- (alg-code "<num,num>") (blank)
     (blank)            -or- (alg-code "send expr <num>(expr**,)") (blank))))
  
  (slide/title
   "Grammar with Compiler-target Cases"
   (prog$*
    "(lexvar-exp)|(apply-method-indexed-exp)"
    "(define the-grammar"
    "  '((program ((arbno class-decl) expression)"
    "             a-program)"
    "    "
    "    (expression (number) lit-exp)"
    "    (expression (\"true\") true-exp)"	
    "    ..."
    "    (expression (\"lexvar\" number number)"
    "      lexvar-exp)"
    "    (expression"
    "      (\"imethod\" expression number"
    "       (separated-list expression \",\"))"
    "      apply-method-indexed-exp)))"))

  (slide/title
   "Interpreter with Compiler-target Cases"
   (scale/improve-new-text
    (prog$*
     "(lexvar-exp)|(apply-method-indexed-exp)"
     "(define (eval-expression exp env)"
     "  (cases expression exp"
     "    (lit-exp (datum) datum)"
     "    (var-exp (id) (apply-env env id))"
     "    ..."
     "    (lexvar-exp (depth pos)"
     "         (apply-env-lexvar env depth pos))"
     "    (apply-method-indexed-exp (obj-exp pos rands)"
     "      (let ((obj (eval-expression obj-exp env))"
     "            (args (eval-rands rands env))"
     "            (c-name (object->class-name obj)))"
     "        (apply-method"
     "          (list-ref"
     "           (class->methods (lookup-class c-name))"
     "           pos)"
     "          ...)))))")
    0.9))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "HW 10"
   (page-para "Homework 10:")
   (page-item "Replace variables with lexical addresses")
   (page-item "Attach field count to" (alg-code "new"))
   (page-item "Index for" (alg-code "initialize") "for" (alg-code "new"))
   (page-item "Index for class, instead of finding by name")
   (page-item "Change" (alg-code "super") "to use class and method index")
   'next
   (page-item "... and more, if you'd like"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
