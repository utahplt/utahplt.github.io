
(module lecture17 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/obj.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "From Functions to Objects"
   (page-item "Functional languages (Scheme, ML)")
   (page-subitem "ADT is a type and a collection of functions")
   (vl-append
    line-sep
    (alg-code "make-fish : (*numT *-> fishT*)")
    (alg-code "grow-fish : (*fishT numT *-> fishT*)")
    (alg-code "fish-size : (*fishT *-> numT*)"))
   'next
   (page-item "Object-oriented languages (Java, C++, Smalltalk)")
   (page-subitem "ADT is a class")
   (vl-append
    line-sep
    (alg-code "fishT class ")
    (alg-code "  method initialize : (*numT *-> *)")
    (alg-code "  method grow : (*numT *-> *)")
    (alg-code "  method size : (* *-> numT*)")))
  
  (slide/title
   "From Functions to Objects"
   (page-para "We can implement objects with functions:")
   (prog*
    "(define (mk-fish size)"
    "  (letrec ([get-size (lambda () size)]"
    "           [grow (lambda (s) "
    "                  (set! size (+ s size)))]"
    "           [eat (lambda (fish)"
    "                 (grow ((fish 'get-size))))])"
    "    (lambda (msg)"
    "      (cond"
    "       [(eq? msg 'get-size) get-size]"
    "       [(eq? msg 'grow) grow]"
    "       [(eq? msg 'eat) eat]))))")
   'next
   (colorize (page-para "but it's not convenient!") RedColor))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Elements of an OO Language"
   
   (page-item "(Expressed) values = objects")
   
   'next
   (vl-append
    line-sep
    (page-item "Classes")
    (page-subitem "superclass")
    (page-subitem "fields")
    (page-subitem "methods"))
   
   'next
   (vl-append
    line-sep
    (page-item "Expression forms")
    (page-subitem "new")
    (page-subitem "method call")
    (page-subitem "super call"))
   
   'next
   (page-item "Program = class declarations + expression"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Syntax"
   (grammar-table
    (list
     (alg-code "prog")        eqls (alg-code "class-decl** expr") (blank)
     (t "") (blank) (blank) (blank)
     (alg-code "class-decl")  eqls (alg-code "class id extends id") (blank)
     (blank) (blank)               (alg-code "   field-decl**") (blank)
     (blank) (blank)               (alg-code "   method-decl**") (blank)
     (alg-code "field-decl")  eqls (alg-code "field id") (blank)
     (alg-code "method-decl") eqls (alg-code "method id(id**,)expr") (blank)
     (t "") (blank) (blank) (blank)
     (alg-code "expr")        eqls (alg-code "new id(expr**,)") (blank)
     (blank)                  -or- (alg-code "send expr id(expr**,)") (blank)
     (blank)                  -or- (alg-code "super id(expr**,)") (blank)
     (blank)                  -or- (alg-code "....") (blank))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define fish-decl
    (alg-code*
     "class fish extends object"
     "  field size"
     "  method initialize (s) set size = s"
     "  method get__size() size"
     "  method grow(food)"
     "    set size = +(size, food)"
     "  method eat(other__fish)"
     "    let s = send other__fish get__size()"
     "     in send self grow(s)"))
  
  (slide/title/tall
   "Example"
   (vl-append
    font-size
    fish-decl
    (alg-code*
     "let f = new fish(10)"
     " in begin"
     "     send f grow(2);"
     "     send f get__size()"
     "    end")))
  
  (slide/title/tall
   "Example"
   (vl-append
    font-size
    fish-decl
    (alg-code*
     "class colorfish extends fish"
     "  field color "
     "  method set__color(c) set color = c"
     "  method get__color() color"
     "....")))
  
  (slide/title/tall
   "Example"
   (vl-append
    font-size
    fish-decl
    (alg-code*
     "...."
     "class pickyfish extends fish"
     "  method grow(food)"
     "    super grow(-(food, 1))"
     "....")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Class Tree"
   (blank)
   (hc-append
    font-size
    (alg-code*
     "class fish ...."
     " "
     "class colorfish"
     "  extends fish "
     " ...."
     " "
     "class pickyfish"
     "  extends fish "
     " ....")
    (text (string (integer->char 222)) '(bold . symbol) font-size)
    (tree #f #f)))
  
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (mk-obj class fields)
    (let ([fields
	   (table
	    3
	    (apply
	     append
	     (map
	      (lambda (x)
		(list (car x) (tt "=") (cadr x)))
	      fields))
	    lc-superimpose lc-superimpose
	    font-size line-sep)])
      (let ([w (max (pict-width class) (pict-width fields))])
	(let ([p (vc-append
		  (* line-sep 2)
		  (colorize class blue)
		  (colorize (linewidth 2 (hline (+ w (* 4 line-sep)) 1))
			    GreenColor)
		  fields)])
	  (color-frame p GreenColor 2)))))
  
  (define (tree-append c m . l)
    (let ([h (pict-height titleless-page)])
      (cc-superimpose
       (lt-superimpose
	titleless-page
	(cc-superimpose
	 (tree c m)
	 (blank (* 1/2 client-w) 1))
	(inset
	 (apply vl-append (* 2 font-size) l)
	 (+ (* 1/2 client-w) font-size)
	 0 0 0))
       (vline 0 h))))
  
  (define (objname s) (it s))
  (define (mk-colorfish size color)
    (mk-obj (alg-code "colorfish")
	    (list (list (alg-code "size") (alg-code size))
		  (list (alg-code "color") (alg-code color)))))
  (define (mk-pickyfish size)
    (mk-obj (alg-code "pickyfish")
	    (list (list (alg-code "size") (alg-code size)))))
  (define (name-fish name fish)
    (ht-append
     (objname name)
     (alg-code " = ")
     fish))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    #f #f 
    (alg-code$ "new .*" "new colorfish(1)")))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    #f #f
    (alg-code "new colorfish(1)")
    (name-fish "obj" (mk-colorfish "1" "0"))))
  
  (define (p1 p)
    (alg-code$*
     p
     "let "
     "o1 = new colorfish(3)"
     "in begin"
     "send o1 grow(4);"
     "send o1 get__size()"
     "end"))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    #f #f
    (p1 "new .*")))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    "fish" "grow"
    (p1 "send o1 grow...")
    (name-fish "o1" (mk-colorfish "3" "0"))))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    "fish" "get__size"
    (p1 "send o1 get__size.*")
    (name-fish "o1" (mk-colorfish "7" "0"))))
  
  (define (p2 p)
    (alg-code$*
     p
     "let "
     "o1 = new colorfish(3)"
     "o2 = new pickyfish(6)"
     "in begin"
     "send o2 eat(o1);"
     "send o2 get__size()"
     "end"))
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    #f #f
    (p2 "new .*")))
  
  (define (pf-eat stage)
    (slide/title/tall
     "Evaluation Sketch"
     (tree-append
      (case stage
	[(0 2) "fish"]
	[(1) "pickyfish"])
      (case stage
	[(0) "eat"]
	[(1 2) "grow"])
      (p2 "send o2 eat.o1.")
      (name-fish "o1" (mk-colorfish "3" "0"))
      (name-fish "o2" (mk-pickyfish "6")))))
  
  (pf-eat 0)
  (pf-eat 1)
  (pf-eat 2)
  
  (slide/title/tall
   "Evaluation Sketch"
   (tree-append
    "fish" "get__size"
    (p2 "send o2 get__size..")
    (name-fish "o1" (mk-colorfish "3" "0"))
    (name-fish "o2" (mk-pickyfish "8"))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (define todo-l '())
  (define (todo-slide/title todo title . l)
    (define old-l todo-l)
    (set! todo-l (append todo-l (map (lambda (x) (prog "[^ ]+ :|#%super" x)) todo)))
    (slide/title
     title
     (apply vc-append font-size l)
     'alts~
     (map (lambda (old?)
	    (list
	     (if (and old? (null? old-l))
		 (blank)
		 (lc-superimpose
		  (blank client-w 0)
		  (htl-append 
		   (apply vl-append 
			  (if old? old-l todo-l)))))))
	  (if (null? todo)
	      '(#t)
	      '(#t #f)))))
  
  (todo-slide/title
   (list "elaborate-class-decls! : lstof-cls-decl -> ")
   "Interpreter"
   (page-item "First, build class tree")
   (prog$* "elab.*!"
	   "(define eval-program "
	   "  (lambda (pgm)"
	   "    (cases program pgm"
	   "      (a-program (c-decls exp)"
	   "        (elaborate-class-decls! c-decls)"
	   "        (eval-expression exp (init-env))))))"))
  
  (todo-slide/title
   (list "new-object : sym -> object"
	 "find-method-and-apply : sym sym object"
	 "                     lstof-expval -> expval")
   "Interpreter"
   (page-item "Expression form: object creation")
   (prog$* "(new-object |find-method-and-apply)"
	   "(new-object-exp (class-name rands)"
	   "  (let ((args (eval-rands rands env))"
	   "        (obj (new-object class-name)))"
	   "    (find-method-and-apply"
	   "      'initialize class-name obj args)"
	   "    obj))"))
  
  (todo-slide/title
   null ; (list "object->class-name : object -> sym")
   "Interpreter"
   (page-item "Expression form: method call")
   (prog$* "find-method-and-apply"
	   "(method-app-exp (obj-exp method-name rands)"
	   "  (let ((args (eval-rands rands env))"
	   "        (obj (eval-expression obj-exp env)))"
	   "    (find-method-and-apply"
	   "      method-name (object->class-name obj) "
	   "      obj args)))"))
  (todo-slide/title
   null ; (list "#%super in method environment")
   "Interpreter"
   (page-item "Expression form: super call")
   (prog$* "(find-method-and-apply)"
	   "(super-call-exp (method-name rands)"
	   "  (let ((args (eval-rands rands env))"
	   "        (obj (apply-env env 'self)))"
	   "    (find-method-and-apply"
	   "      method-name (apply-env env '%super)"
	   "      obj args)))"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Class Elaboration"
   (page-item "Elaboration can just keep the declarations")
   (hc-append
    font-size
    (alg-code*
     "class fish ...."
     ""  
     "class colorfish"
     "  extends fish "
     " ...."
     "  "
     "class pickyfish"
     "  extends fish "
     " ....")
    (tt "=")
    (tree #f #f)))
  
  (slide/title
   "Class Elaboration"
   (prog$* "elaborate-class-decls!"
	   "(define the-class-env '())"
	   ""
	   "(define (elaborate-class-decls! c-decls)"
	   "    (set! the-class-env c-decls)))"))
  
  
  (slide/title
   "Class Elaboration"
   (page-item "Finding a node in the tree:")
   (prog*
    ";; lookup-class : sym -> class-decl"
    "(define (lookup-class name)"
    "  (lookup name the-class-env))"
    " "
    ";; lookup : sym lstof-cls-decl -> class-decl"
    "(define (lookup-class-in-env name env)"
    "  (cond"
    "   [(null? env) "
    "    (eopl:error 'lookup-class "
    "                \"Unknown class ~s\" name)]"
    "   [(eqv? (class-decl->class-name (car env))"
    "          name) "
    "    (car env)]"
    "   [else (lookup name (cdr env))]))"))
  
  (define cf-five (alg-code "5"))
  (define cf-three (alg-code "3"))
  
  (define a-colorfish 
    (mk-parts `(("colorfish" ,cf-five)
		("fish" ,cf-three))))
  
  (define a-dietfish
    (mk-parts '(("dietfish" "10" "18" "12")
		("pickyfish")
		("fish" "8"))))
  
  (slide/title
   "Object Representation"
   (page-item "An object = a list of" (dt "parts"))
   (page-subitem "from instantiated class up to base class")
   'alts
   (list (list
	  a-colorfish)
	 (list
	  (mk-parts '(("pickyfish")
		      ("fish" "7"))))
	 (list
	  (lc-superimpose
	   (blank client-w 0)
	   (ht-append
	    font-size
	    (alg-code*
	     "class dietfish"
	     "  extends pickyfish"
	     "  field carbos"
	     "  field sodium"
	     "  field cholesterol"
	     "  ....")
	    a-dietfish))))
   'next
   (page-item "Use part vectors in environments"))
  
  (define df-part (mk-part "dietfish" '("10" "18" "12")))
  (define df-list
    (mk-parts `(("dietfish" "10" "18" "12")
		,(tt "..."))))
  
  (slide/title
   "Object Representation"
   (prog*
    "(define-datatype part part? "
    "  (a-part"
    "    (class-name symbol?)"
    "    (fields vector?)))")
   df-part
   'next
   (blank)  
   (prog* ";; An object is a list of parts")
   df-list)
  
  (slide/title
   "Object Representation"
   (prog$* " new-object|make-first-part"
	   ";; new-object : sym -> object"
	   "(define (new-object cls-name)"
	   "  (if (eqv? cls-name 'object)"
	   "     '()"
	   "     (let ([c-decl (lookup-class cls-name)])"
	   "       (cons"
	   "        (make-first-part c-decl)"
	   "        (new-object (class-decl->super-name "
	   "                     c-decl)))))))")
   df-list)
  
  (slide/title
   "Object Representation"
   (prog$* " make-first-part"
	   ";; make-first-part : class-decl -> part"
	   "(define (make-first-part c-decl)"
	   "  (a-part"
	   "   (class-decl->class-name c-decl)"
	   "   (make-vector "
	   "    (length (class-decl->field-ids "
	   "             c-decl)))))")
   df-part)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  (slide/title/tall
   "Method Search"
   (page-item (alg-code "get__size") "in" (alg-code "colorfish") ":"
	      "Check" (alg-code "colorfish") "'s methods,"
	      "then methods in the superclass" (alg-code "fish") ", etc.")
   (tree #f #f))
  
  (slide/title
   "Method Search"
   (prog$* " find-method-and-apply|apply-method"
	   "(define find-method-and-apply"
	   "  (lambda (m-name host-name self args)"
	   "    (if (eqv? host-name 'object)"
	   "      (eopl:error ...) ; not found"
	   "      (let ([m-decl "
	   "             (lookup-method-decl"
	   "              m-name"
	   "              (class-name->method-decls"
	   "               host-name))])"
	   "        (if (method-decl? m-decl)"
	   "            (apply-method m-decl host-name"
	   "			  self args)"
	   "            (find-method-and-apply m-name"
	   "	     (class-name->super-name "
	   "	      host-name)"
	   "             self args))))))"))
  
  (define (mk-find-x find)
    (lambda (b p)
      (let-values ([(x y) (find b p)])
	(values (+ x (/ font-size 2)) y))))
  (define find-xt (mk-find-x find-lt))
  (define find-xb (mk-find-x find-lb))
  
  (define (env-frame bindings)
    (let ([rw (apply max (map pict-width (map car bindings)))]
	  [lw (apply max (map pict-width (map cadr bindings)))])
      (apply
       vc-append
       -1
       (map
	(lambda (b)
	  (array blue 
		 (lc-superimpose (car b) (blank rw 0))
		 (if (eq? (cadr b) dash-placeholder)
		     dash-placeholder
		     (lc-superimpose (cadr b) (blank lw 0)))))
	bindings))))
  
  (define (ts-array n)
    (let ([t (alg-code "self")]
	  [s (alg-code "%super")]
	  [v (alg-code "....")])
      (env-frame (list (list n v)
		       (list t v)
		       (list s v)))))
  
  (define empty-env (colorize (circle (* 3/4 font-size)) blue))
  
  (define (mk-mapp env? color?)
    (slide/title/tall
     "Method Application"
     (ht-append
      (* 3 font-size)
      (if color?
	  (tree "colorfish" "set__color")
	  (tree "fish" "grow"))
      (if (not env?)
	  a-colorfish
	  (let* ([size-frame (array blue (alg-code "size") dash-placeholder)]
		 [color-frame (array blue (alg-code "color") dash-placeholder)]
		 [f-frame (ts-array (alg-code "f"))]
		 [c-frame (ts-array (alg-code "c"))]
		 [sep font-size]
		 [asize (/ font-size 2)]
		 [p (if color?
			(vl-append
			 sep
			 size-frame
			 color-frame
			 c-frame)
			(vl-append
			 sep
			 size-frame
			 f-frame))])
	    (let ([p2 (add-arrow-line
		       asize
		       (add-arrow-line
			asize
			(vc-append
			 (* 2 sep)
			 a-colorfish
			 (vl-append
			  sep
			  empty-env
			  p))
			(list size-frame dash-box) find-ct
			cf-three find-cb
			2 blue)
		       size-frame find-xt
		       empty-env find-cb
		       2 blue)])
	      (if color?
		  (add-arrow-line
		   asize
		   (add-arrow-line
		    asize
		    (add-arrow-line
		     asize
		     p2
		     color-frame find-xt
		     size-frame find-xb
		     2 blue)
		    (list color-frame dash-box) find-ct
		    cf-five find-cb
		    2 blue)
		   c-frame find-xt
		   color-frame find-xb
		   2 blue)
		  (add-arrow-line
		   asize
		   p2
		   f-frame find-xt
		   size-frame find-xb
		   2 blue))))))))
  
  (mk-mapp #f #f)
  (mk-mapp #t #f)
  (mk-mapp #t #t)
  
  (slide/title
   "Method Application"
   (scale/improve-new-text
    (prog$* " apply-method|view-object-as|build-field-env|eval-expression"
	    ";; apply-method : method-decl sym object "
	    ";;                lstof-expval -> expval"
	    "(define apply-method"
	    "  (lambda (m-decl host-name self args)"
	    "    (let ([ids (method-decl->ids m-decl])"
	    "	  [body (method-decl->body m-decl])"
	    "	  [super-name "
	    "	   (class-name->super-name host-name)])"
	    "      (eval-expression "
	    "       body"
	    "       (extend-env"
	    "	(cons '%super (cons 'self ids))"
	    "	(cons super-name (cons self args))"
	    "	(build-field-env "
	    "	 (view-object-as self "
	    "			 host-name)))))))")
    0.9))
  
  (slide/title
   "Method Application"
   (prog$* "extend-env-refs"
	   ";; view-object-as : object sym -> lstof-parts"
	   "(define (view-object-as parts class-name)"
	   " (if (eqv? (part->class-name (car parts)) "
	   "	   class-name)"
	   "    parts"
	   "    (view-object-as (cdr parts) class-name)))"
	   " " 
	   ";; build-field-env : lstof-parts -> env"
	   "(define (build-field-env parts)"
           "  (if (null? parts)"
	   "      (empty-env)"
	   "      (extend-env-refs"
	   "       (part->field-ids (car parts))"
	   "       (part->fields    (car parts))"
	   "       (build-field-env (cdr parts)))))"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Object Implementation Overview"
   (page-item (bt "Inheritance") ": superclass chain for fields and methods, part chain")
   (page-item (bt "Overriding") ": method dispatch uses object tag")
   (page-item (bt "Super calls") ":" (alg-code "%super") "hidden variable contains superclass name"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)
