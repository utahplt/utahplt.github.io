
(module lecture18 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/obj.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/center
   (let ([size (* 1/4 client-h)]
	 [type (lambda (p t)
		 (hc-append (alg-code "{ } |- ")
			    p
			    (alg-code " : ")
			    (colorize (tt t) GreenColor)))])
     (infer (type (jack-o-lantern size) "jack-o-lantern")
	    (ante-append
	     (type (jack-o-lantern size "orange" "orange") "pumpkin")
	     (type (jack-o-lantern size "white" "black" "white") "face")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Object Implementation Overview"
   (page-item (bt "Inheritance") ": superclass chain for fields and methods, part chain for objects")
   (page-item (bt "Overriding") ": method dispatch uses object tag")
   (page-item (bt "Super calls") ":" (alg-code "%super") "hidden variable contains superclass name"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Copied from lecture18 !!!!!!!!!!!!!!!!!
  
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
	     "  field cholestorol"
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
	   "  (if (eq? cls-name 'object)"
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
  
  (slide/title
   "Method Search"
   (prog$* " find-method-and-apply|apply-method"
	   "(define find-method-and-apply"
	   "  (lambda (m-name host-name self args)"
	   "    (if (eq? host-name 'object)"
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
	   "			 host-name)))))))"))
  
  (slide/title
   "Method Application"
   (prog$* "extend-env-refs"
	   ";; view-object-as : object sym -> lstof-part"
	   "(define (view-object-as parts class-name)"
	   " (if (eq? (part->class-name (car parts)) "
	   "          class-name)"
	   "   parts"
	   "   (view-object-as (cdr parts) class-name)))"
	   " " 
	   ";; build-field-env : lstof-parts -> env"
	   "(define (build-field-env parts)"
	   "  (if (null? parts)"
	   "      (empty-env)"
	   "      (extend-env-refs"
	   "       (part->field-ids (car parts))"
	   "       (part->fields    (car parts))"
	   "       (build-field-env (cdr parts)))))"))
  
  (slide/title
   "Complete Implementation"
   (page-para* "(implement in DrScheme)"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define flat-dietfish (mk-flat-obj #f "dietfish" '("8" "10" "18" "12")))
  (define flat-colorfish (mk-flat-obj #f "colorfish" `(,cf-three ,cf-five)))
  
  (define (realistic color?)
    (slide/title
     "A More Realistic Object Representation"
     (page-item "A chain of parts wastes space")
     (page-item "Collapse vectors into one")
     (blank)
     (ht-append
      (* 4 font-size)
      (if color? a-colorfish a-dietfish)
      (text (string (integer->char 222)) '(bold . symbol) font-size)
      (if color? flat-colorfish flat-dietfish))))
  
  (realistic #t)
  (realistic #f)
  
  (slide/title
   "A More Realistic Object Representation"
   (prog$* "roll-up-field-length"
           "(define-datatype object object? "
           "                 (an-object"
           "                  (class-name symbol?)"
           "                  (fields vector?)))"
           " "
           ";; new-object : sym -> object"
           "(define (new-object class-name)"
           "  (an-object"
           "   class-name"
           "   (make-vector"
           "    (roll-up-field-length class-name))))"))
  
  (slide/title
   "A More Realistic Object Representation"
   (prog*
    ";; roll-up-field-length : sym -> num"
    "(define roll-up-field-length "
    "  (lambda (class-name)"
    "    (if (eqv? class-name 'object)"
    "        0"
    "        (+ (roll-up-field-length"
    "            (class-name->super-name "
    "             class-name))"
    "           (length "
    "            (class-name->field-ids "
    "             class-name))))))"))
  
  (define (mk-mapp2 env? color?)
    (define -flat-colorfish (inset flat-colorfish (* 3 font-size) 0 0 0))
    (slide/title/tall
     "Method Application with Flat Objects"
     (ht-append
      (* 3 font-size)
      (if color?
          (tree "colorfish" "set__color")
          (tree "fish" "grow"))
      (if (not env?)
          -flat-colorfish
          (let* ([frame (env-frame (cons
                                    (list (alg-code "size") dash-placeholder)
                                    (if color?
                                        (list 
                                         (list (alg-code "color") dash-placeholder))
                                        null)))]
                 [f-frame (ts-array (alg-code "f"))]
                 [c-frame (ts-array (alg-code "c"))]
                 [sep font-size]
                 [asize (/ font-size 2)]
                 [p (vl-append
                     sep
                     frame
                     (if color?
                         c-frame
                         f-frame))])
            (let ([p2 (add-arrow-line
                       asize
                       (add-arrow-line
                        asize
                        (vc-append
                         (* 2 sep)
                         -flat-colorfish
                         (vl-append
                          sep
                          empty-env
                          p))
                        dash-box find-ct
                        cf-three find-cb
                        2 blue)
                       frame find-xt
                       empty-env find-cb
                       2 blue)])
              (if color?
                  (add-arrow-line
                   asize
                   p2
                   c-frame find-xt
                   frame find-xb
                   2 blue)
                  (add-arrow-line
                   asize
                   p2
                   f-frame find-xt
                   frame find-xb
                   2 blue))))))))
  
  (mk-mapp2 #f #f)
  (mk-mapp2 #t #f)
  (mk-mapp2 #t #t)
  
  (slide/title
   "Method Application with Flat Objects"
   (prog$* "roll-up-field-ids"
           "(define apply-method"
           "  (lambda (m-decl host-name self args)"
           "    (let ([ids (method-decl->ids m-decl)]"
           "          [body (method-decl->body m-decl)]"
           "          [super-name (class-name->super-name "
           "                       host-name)]"
           "          [field-ids (roll-up-field-ids "
           "                      host-name)]"
           "          [fields (object->fields self)])"
           "      (eval-expression "
           "       body"
           "       (extend-env"
           "        (cons '%super (cons 'self ids))"
           "        (cons super-name (cons self args))"
           "        (extend-env-refs field-ids fields "
           "                         (empty-env)))))))"))
  
  (slide/title
   "Complete Implementation of Flat Objects"
   (page-para* "(implement in DrScheme)"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "A More Realistic Class Representation"
   (page-para (colorize (bt "Eliminate tree walks") blue)
              ": object creation, method calls")
   (tree #f #f))

  (slide/title
   "Object Creation without Tree Walks"
   (page-para "Current interpreter:")
   (page-para " 1. Find class")
   (page-para " 2. Get field list (walk tree)")
   (page-para " 3. Allocate field array and object")
   'next
   (blank)
   (page-para "To eliminate tree walks:")
   (colorize (page-para " 2. Extract flat field list from class")
             RedColor))

  (slide/title
   "Method Calls without Tree Walks"
   (page-para "After object and arguments are determined:")
   (page-para " 1. Lookup object class")
   (page-para " 2. Find class containing method (walk tree)")
   (page-para " 3. Get field variables for class (walk tree)")
   (page-para " 4. Create environment: fields +" (alg-code "%super") "+" (alg-code "self") "+ args")
   (page-para " 5. Evaluate method body")
   'next
   (blank)
   (page-para "To eliminate tree walks:")
   (colorize (page-para " 2 & 3. Find method in current class, extract field list and superclass name")
             RedColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Class Elaboration"
   'alts
   (list (list
          (hc-append
           font-size
           (tree #f #f)
           (colorize (arrow font-size 0) green)))
         (list
          (scale (tree2 #f #f) 0.8))))

  (define (elab-slide p)
    (slide/title
     "Class Elaboration"
     (scale (tree2 #f (colorize p purple)) 0.8)))

  (elab-slide (alg-code "new colorfish(3)"))
  (elab-slide (alg-code "send cf get__size()"))

  (slide/title
   "Implementation"
   (page-item "Change" (tt "elaborate-class-decls!") "to build annotated tree")
   (page-item "Change" (tt "new-object") "to use class's immediate field list")
   (page-item "Change" (tt "apply-method") "to work with annotated methods")
   'next
   (blank)
   (page-para* "(implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)
