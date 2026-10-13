
;; Unusual indentation needed:
;;   syntax-id-rules
;;   let^1
;;   let-one

(module talk (lib "run.ss" "slideshow")
  (require (lib "code.ss" "slideshow")
	   (lib "step.ss" "slideshow")
	   (lib "math.ss")
	   (lib "class.ss")
	   (lib "mred.ss" "mred"))
  (require-for-syntax (lib "stx.ss" "syntax"))
  
  (current-keyword-list (append (current-keyword-list)
				(list
				 "syntax-id-rules")))
  (define GreenColor "green")
  (define DarkGreenColor "forest green")
  (define BlueColor "blue")
  (define RedColor "red")
  (define BeigeColor (make-object color% 255 255 200))

  (define (symbol n)
    (text (string (integer->char n)) 'symbol font-size))

  (define sym:in (symbol 206))
  (define sym:rightarrow (symbol 174))
  (define sym:infinity (symbol 165))
  (define sym:times (symbol 180))
  (define sym:implies (symbol 222))

  (define dt bit)

  (define (outline-subitem . l)
    (lambda (x)
      (apply item (* 3/4 client-w) l)))

  (define outline
    (make-outline
     'general "Macros In General" #f
     'patterns "Pattern-Based Macros" (outline-subitem "Scheme macro basics")
     'example "Extended Example" (outline-subitem "Using patterns"
						  "and macro-generating macros")
     'lexical "Lexical Scope" (outline-subitem "Making it work")
     'transformers "General Transformers" (outline-subitem "Beyond patterns and templates")
     'art "State of the Art" (outline-subitem "Scheme's present and near future")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/center
   (vc-append
    line-sep
    (titlet "Scheme-Style Macros: Patterns and Lexical Scope"))
   (blank)
   (bitmap (build-path (collection-path "icons") "PLTnolarval.jpg"))
   (blank)
   (vc-append
    line-sep
    (colorize (t "Matthew Flatt") "blue")
    (blank font-size)
    (t "University of Utah")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Makes the book 3D
  (define (stack p)
    (let ([w (pict-width p)]
	  [h (pict-height p)])
      (lt-superimpose
       (inset
	(let loop ([n 8])
	  (if (zero? n)
	      (frame (blank w h))
	      (lt-superimpose
	       (inset (loop (sub1 n)) (/ w 60) (/ h 60) 0 0)
	       (frame (colorize (filled-rectangle w h) "white")))))
	(/ w 60) (/ h 60) 0 0)
       (frame p))))

  (slide/title
   "Why Macros?"
   (colorize
    (page-para* "Language designers have to stop somewhere")
    BlueColor)
   (hb-append gap-size 
	      (stack (bitmap "jls.jpg"))
	      (t "(544 pages)"))
   'next
   (blank)
   (page-para* "No language can provide every possible useful construct")
   (page-para* "Macros let a programmer fill in gaps"))

  (define versus
    (colorize (it "versus") BlueColor))
  
  (define detail-line
    (colorize (hline (* 3/4 client-w) 1) DarkGreenColor))
  
  (define ext-file-icon (let ([p (t ".class")])
                          (file-icon (* 5/4 (pict-width p))
                                     (* 7/4 (pict-width p))
                                     BeigeColor)))
  (define step-arrow (colorize (arrow gap-size 0) GreenColor))
  
  (define together-arrows
    (colorize
     (vc-append
      gap-size
      (arrow gap-size (* pi -1/4))
      (arrow gap-size (* pi 1/4)))
     GreenColor))

  (define (mk-file name ext)
    (cc-superimpose ext-file-icon (vc-append
				   (t name)
				   (t ext))))

  (slide/title
   "Macros versus Arbitrary Program Generators"
   (page-para* "Macros extend the language without extending the tool chain")
   'next
   detail-line
   (blank)
   'alts
   (list (list
	  (page-para "Jack (YACC for Java) requires a new tool chain:")
	  (blank)
	  (hc-append
	   gap-size
	   (vr-append
	    gap-size
	    (hc-append gap-size
		       (mk-file "Grammar" ".jack")
		       (vc-append (tt "jack")
				  step-arrow)
		       (mk-file "Grammar" ".class"))
	    (hc-append gap-size
		       (mk-file "Run" ".java")
		       (vc-append (tt "javac")
				  step-arrow)
		       (mk-file "Run" ".class")))
	   together-arrows
	   (mk-file "Interp" ".jar"))
	  'next
	  (blank)
	  (colorize
	   (page-para* sym:implies "Jack doesn't play nice with all Java environments")
	   BlueColor))
	 (list
	  (page-para "Scheme-YACC is a macro:")
	  (blank)
	  (hc-append gap-size
		     (mk-file "SYACC" ".scm")
		     (vc-append
		      gap-size
		      (mk-file "Grammar" ".scm")
		      (mk-file "Run" ".scm"))
		     (vc-append (tt "mzc") (hc-append step-arrow together-arrows))
		     (mk-file "Interp" ".exe"))
	  'next
	  (blank)
	  (colorize
	   (page-para* sym:implies "SYACC automatically plays nice with all Scheme environments")
	   BlueColor)
	  'next
	  (page-para/c
	   (scale/improve-new-text
	    (colorize
	     (t "... in principle")
	     RedColor)
	    0.6 0.6)))))

  (slide/title
   "Macros and Libraries"
   (page-item "Macros = hook in tool chain to extend a language")
   (blank)
   (colorize
    (page-para* "Scheme ensures that macros play nice with the tool chain")
    BlueColor)
   (blank)
   'next
   (blank)
   (blank)
   (page-item "Some libraries include macros")
   (blank)
   (colorize
    (page-para* "Scheme ensures that library macros play nice with each other")
    BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'patterns)

  (define doc-w (* 3/4 client-w))

  (define plus-bullet (colorize (tt "+") GreenColor))
  (define minus-bullet (colorize (tt "-") RedColor))

  (define expands (colorize sym:implies BlueColor))

  (define (same-ad-as p p1)
    (lift (drop p1 (- (pict-ascent p))) (- (pict-descent p))))
  
  (define .....-val (let ([p (code .....)])
		      (same-ad-as p (cloud (pict-width p) (pict-height p)))))

  
  (slide/title
   "Pattern-Based Macros"
   (page-para "Most popular API for macros:" (dt "patterns"))
   (vl-append
    gap-size
    (tt "#define swap(x, y) (tmp=y, y=x, x=tmp)")
    (vl-append line-sep
	       (tt "swap(c.red, d->blue)") 
	       expands
	       (tt "(tmp=d->blue, d->blue=c.red, c.red=tmp)")))
   'next
   (blank)
   (blank)
   (page-item/bullet plus-bullet "Relatively easy for programmers to understand")
   (page-item/bullet plus-bullet "Obvious hook into the tool chain")
   'next
   (blank)
   (page-item/bullet minus-bullet "Pure patterns can't easily express much")
   (page-para/r "...but possibly more than you think"))

  (define (color-box p color)
    (cc-superimpose
     (colorize
      (dc (lambda (dc x y)
            (send dc draw-rectangle x y (pict-width p) (pict-height p)))
          (pict-width p) (pict-height p) (pict-ascent p) (pict-descent p))
      color)
     p))
  (define light-shade (make-object color% 195 195 255))
  (define (darker c)
    (scale-color 0.95 c))

  (define pattern (color-box (it "pattern") light-shade))
  (define template (color-box (it "template") (darker light-shade)))

  (define cloud-shade (make-object color% 195 255 195))
  (define (encloud p shade)
    (cc-superimpose
     (cloud (* 9/8 (pict-width p))
	    (* 3/2 (pict-height p))
	    shade)
     p))

  ;; The scode macro is like code, except that it
  ;;   - Shades behind patterns and templates (by recognizing keywords)
  ;;   - Puts clouds around expr for (code:encloud expr)
  ;; Much of the complexity has to do with preserving source-location
  ;;  info, which is crucial to proper typesetting of the code
  ;; >>> Some cut and paste here should be cleaned up! <<<
  (define-syntax (scode stx)
    (syntax-case stx ()
      [(_ stx ...)
       #`(code
          #,@(map
              (lambda (stx)
                (let loop ([stx stx][light-shade #'light-shade][cloud-shade #'cloud-shade])
                  (syntax-case stx (..... code:encloud)
		    [(ds n b)
		     (and (identifier? #'ds)
			  (module-identifier=? #'ds #'define-syntax))
		     (datum->syntax-object
		      stx
		      (list
		       #'ds
		       (loop #'n light-shade cloud-shade)
		       (loop #'b light-shade cloud-shade))
		      stx)]
                    [(sr kws pat--tmpl ...)
                     (and (identifier? #'sr)
                          (or (module-identifier=? #'sr #'syntax-rules)
                              (module-identifier=? #'sr #'syntax-id-rules)))
                     (let ([pat--tmpls (syntax->list #'(pat--tmpl ...))])
                       (datum->syntax-object
                        stx
                        (list*
                         #'sr
                         #'kws
                         (map (lambda (pt)
				(if (stx-pair? pt)
				    (let ([tmpl (loop (stx-car (stx-cdr pt)) 
						      #`(darker (darker #,light-shade))
						      cloud-shade)]
					  [pat (stx-car pt)])
				      (with-syntax ([us #'unsyntax])
					(datum->syntax-object
					 pt
					 (list
					  #`(us (datum->syntax-object
						 #f
						 (color-box (code #,pat) #,light-shade)
						 (quote-syntax #,pat)))
					  #`(us (datum->syntax-object
						 #f
						 (color-box (code #,tmpl) (darker #,light-shade))
						 (quote-syntax #,(stx-car (stx-cdr pt))))))
					 pt)))
				    (loop pt light-shade cloud-shade)))
                              pat--tmpls))
                        stx))]
		    [(sc expr kws pat--expr ...)
                     (and (identifier? #'sc)
			  (module-identifier=? #'sc #'syntax-case))
		     (let ([pat--exprs (syntax->list #'(pat--expr ...))])
                       (datum->syntax-object
                        stx
                        (list*
                         #'sc
			 (loop #'expr light-shade cloud-shade)
                         #'kws
                         (map (lambda (pe)
				(if (stx-pair? pe)
				    (let ([expr (stx-car (stx-cdr pe))]
					  [pat (stx-car pe)])
				      (with-syntax ([us #'unsyntax])
					(datum->syntax-object
					 pe
					 (list
					  #`(us (datum->syntax-object
						 #f
						 (color-box (code #,pat) #,light-shade)
						 (quote-syntax #,pat)))
					  (loop expr light-shade cloud-shade))
					 pe)))
				    (loop pe light-shade cloud-shade)))
                              pat--exprs))
                        stx))]
		    [(ws pat--expr body)
                     (and (identifier? #'ws)
			  (module-identifier=? #'ws #'with-syntax))
		     (let ([pat--exprs (syntax->list #'pat--expr)])
                       (datum->syntax-object
                        stx
                        (list
                         #'ws
			 (datum->syntax-object
			  #'pat--expr
			  (map (lambda (pe)
				 (if (stx-pair? pe)
				     (let ([expr (stx-car (stx-cdr pe))]
					   [pat (stx-car pe)])
				       (with-syntax ([us #'unsyntax])
					 (datum->syntax-object
					  pe
					  (list
					   #`(us (datum->syntax-object
						  #f
						  (color-box (code #,pat) #,light-shade)
						  (quote-syntax #,pat)))
					   (loop expr light-shade cloud-shade))
					  pe)))
				     (loop pe light-shade cloud-shade)))
			       pat--exprs)
			  #'pat--expr)
			 (loop #'body light-shade cloud-shade))
                        stx))]
		    [(sx tmplt)
                     (and (identifier? #'sx)
			  (module-identifier=? #'sx #'syntax))
		     (let ([tmpl (loop #'tmplt
				       #`(darker (darker #,light-shade))
				       cloud-shade)])
		       (datum->syntax-object
			stx
			(list
			 #'sx
			 (with-syntax ([us #'unsyntax])
			   #`(us (datum->syntax-object
				  #f
				  (color-box (code #,tmpl) (darker #,light-shade))
				  (quote-syntax tmplt)))))
			stx))]
		    [(code:encloud x)
		     (with-syntax ([us #'unsyntax])
		       #`(us (datum->syntax-object #f (encloud (code #,(loop #'x light-shade
									     #`(darker #,cloud-shade)))
							       #,cloud-shade)
						   (quote-syntax #,stx))))]
                    [(a . b)
                     (datum->syntax-object 
                      stx
                      (cons (loop #'a light-shade cloud-shade) (loop #'b light-shade cloud-shade))
                      stx)]
		    [..... 
		     (with-syntax ([us #'unsyntax])
		       #`(us (datum->syntax-object #f .....-val (quote-syntax #,stx))))]
                    [x #'x])))
              (syntax->list #'(stx ...))))]))

  (define (introduced id)
    (colorize (parameterize ([code-colorize-enabled #f])
		(typeset-code id))
	      RedColor))

  (define (expands-table . l)
    (table 3
           l
           ltl-superimpose ltl-superimpose
           gap-size line-sep))

  (define swap-defn
    (scode
     (define-syntax swap
       (syntax-rules ()
         [(swap a b) (let ([tmp b])
                       (set! b a)
                       (set! a tmp))]))))

  (with-steps
   (ds rs pat-tmpl pattern template)
   (slide/title
    "Scheme Macro Basics"
    (lt-superimpose
     ((vbetween ds ds)
      (scode
       (define-syntax swap
	 .....)))
     ((vbetween rs rs)
      (scode
       (define-syntax swap
	 (syntax-rules ()
	   .....))))
     ((vbetween pat-tmpl pat-tmpl)
      (code
       (define-syntax swap
	 (syntax-rules ()
	   [#,pattern #,template]
	   ...
	   [#,pattern #,template]))))
     ((vbetween pattern pattern)
      (scode
       (define-syntax swap
	 (syntax-rules ()
	   [(swap a b) .....]))))
     ((vafter template)
      swap-defn))
    (blank)
    (ct-superimpose
     ((vbetween ds ds)
      (page-item (code define-syntax) "indicates a macro definition"))
     ((vbetween rs rs)
      (vc-append
       gap-size
       (page-item (code syntax-rules) "means a pattern-matching macro")
       (page-item (code ()) "means no keywords in the patterns")))
     ((vbetween pat-tmpl pat-tmpl)
      (vc-append
       gap-size
       (page-item "Any number of" (hbl-append pattern (t "s")) "to match")
       (page-item "Produce result from" template "of first match")))
     ((vbetween pattern pattern)
      (vc-append
       gap-size
       (page-item "Just one pattern for this macro:" (code (swap a b)))
       (page-item "Each identifier matches anything in use")
       (expands-table
        (code (swap x y))
        expands
        (page-para* (code a) (t "is") (code x))
        (blank)(blank)
        (page-para* (code b) (t "is") (code y))
        (tt " ")(blank)(blank)
        (code (swap 9 (+ 1 7)))
        expands
        (page-para* (code a) (t "is") (code 9))
        (blank)(blank)
        (page-para* (code b) (t "is") (code (+ 1 7))))))
     ((vbetween template template)
      (vc-append
       gap-size
       (page-item "Bindings substituted into template to generate the result")
       (expands-table
        (code (swap x y))
        expands
        (code (let ([tmp y])
                (set! y x)
                (set! x tmp)))
        (tt " ")(blank)(blank)
        (code (swap 9 (+ 1 7)))
        expands
        (code (let ([tmp (+ 1 7)])
                (set! (+ 1 7) 9)
                (set! 9 tmp)))))))))

  (with-steps
   (setup setupx lexical)
   (slide/title
    "Lexical Scope"
    swap-defn
    (blank)
    (vc-append
     gap-size
     (page-item "What if we" (code swap) "a variable named" (code tmp) "?")
     (expands-table
      (code (let ([tmp 5]
		  [other 6])
	      (swap tmp other)))
      (if (between? setup setupx)
	  (vc-append (- line-sep) (colorize (t "?") RedColor) expands)
	  expands)
      (if (between? setup setupx)
	  (code (let ([tmp 5]
		      [other 6])
		  (let ([tmp other])
		    (set! other tmp)
		    (set! tmp tmp))))
	  (code (let ([tmp 5]
		      [other 6])
		  (let ([tmp_1 other])
		    (set! other tmp)
		    (set! tmp tmp_1)))))))
    (lt-superimpose
     ((vbetween setupx setupx)
      (colorize (page-para* (it "This expansion would violate lexical scope")) RedColor))
     ((vafter lexical)
      (colorize (page-para* "Scheme renames the introduced binding") BlueColor)))))

  (define rename-slide (retract-most-recent-slide))

  (define details-later (colorize (it "Details later...") RedColor))

  (re-slide rename-slide (rb-superimpose
			  full-page
			  details-later))

  (slide/title
   "Lexical Scope: Local Bindings"
   (page-para "Lexical scope means that local macros work, too:")
   (scode (define (f x)
	     (define-syntax swap-with-arg
	       (syntax-rules ()
		 [(swap-with-arg y) (swap x y)]))
	     (code:encloud
	      (let ([z 12]
		    [x 10])
		(code:comment "Swaps z with original x:")
		(swap-with-arg z)))))
   (blank)
   (blank)
   (page-para/r details-later))

  (define seq-title "Matching Sequences")

  (slide/title
   seq-title
   (page-para "Some macros need to match sequences")
   (vl-append
    gap-size
    (code (rotate x y))
    (code (rotate red green blue))
    (code (rotate front-left
                  rear-right
                  front-right
                  rear-left))))
  
  (slide/title
   seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a) (void)]
              [(rotate a b c ...) (begin
                                    (swap a b)
                                    (rotate b c ...))])))
   (blank)
   (page-item (code ...) "in a pattern: multiple of previous sub-pattern")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(page-para* (code c) "is" (code z w)))
	  ltl-superimpose ltl-superimpose
	  gap-size line-sep)
   'next
   (page-item (code ...) "in a template: multiple instances of previous sub-template")
   (table 3
	  (list (code (rotate x y z w))
		expands
		(code (begin
			(swap x y)
			(rotate y z w))))
	  ltl-superimpose ltl-superimpose
	  gap-size line-sep))

  (slide/title
   seq-title
   (scode (define-syntax rotate
            (syntax-rules ()
              [(rotate a c ...) 
               (shift-to (c ... a) (a c ...))]))
          code:blank
          (define-syntax shift-to
            (syntax-rules ()
              [(shift-to (from0 from ...) (to0 to ...)) 
               (let ([tmp from0])
                 (set! to from) ...
                 (set! to0 tmp))])))
   (blank)
   (page-item (code ...) "maps over same-sized sequences")
   (page-item (code ...) "duplicates constants paired with sequences"))

  (slide/title
   "Identifier Macros"
   (page-para "The" (code swap) "and" (code rotate) "names work only"
              "in an \"application\" position")
   (expands-table
    (scode (swap x y)) expands (scode (let ([tmp y]) .....))
    (scode (+ swap 2)) expands (colorize (it "syntax error") RedColor))
   'next
   (blank)
   (page-para "An" (dt "identifier macro") "works in any expression position")
   (expands-table
    (code clock) expands (code (get-clock))
    (code (+ clock 10)) expands (code (+ (get-clock) 10))
    (code (clock 5)) expands (code ((get-clock) 5)))
   'next
   (page-para "...or as a" (code set!) "target")
   (expands-table
    (code (set! clock 10)) expands (code (set-clock! 10))))

   
  (slide/title
   "Identifier Macros"
   (scode (define-syntax clock
            (syntax-id-rules (set!)
              [(set! clock e) (put-clock! e)]
              [(clock a ...) ((get-clock) a ...)]
              [clock (get-clock)])))
   (blank)
   (page-item (code set!) "is designated as a keyword")
   (page-item (code syntax-rules) "is a special case of" (code syntax-id-rules)
              "with errors in the first and third cases"))

  (define define-get/put-id-defn
    (scode
     (define-syntax define-get/put-id
       (syntax-rules ()
	 [(define-get/put-id id get put!)
	  (define-syntax id
	    (syntax-id-rules (set!)
	      [(set! id e) (put! e)]
	      [(id a (... ...)) 
	       ((get) a (... ...))]
	      [id (get)]))]))))

  (slide/title
   "Macro-Generating Macros"
   (page-para "If we have many identifiers like" (code clock) "...")
   'next
   (vl-append
    define-get/put-id-defn
    (scode code:blank
	   (define-get/put-id clock get-clock put-clock!)))
   (blank)
   (page-item (code (... ...)) "in a template gets replaced by" (code ...)))
    

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'example)
  
  (define cbr-code
    (code (define-cbr (f a b)
            (swap a b))
          code:blank
          (let ([x 1] [y 2])
            (f x y)
            x)))
   
  (slide/title
   "Extended Example"
   (page-para "Let's add call-by-reference definitions to Scheme")
   (vl-append
    line-sep
    cbr-code
    (code (code:comment "should produce 2"))))
  
  (define cbr-swap-expansion
    (code (define (do-f get-a get-b put-a! put-b!)
            (define-get/put-id a get-a put-a!)
            (define-get/put-id b get-b put-b!)
            (swap a b))))
  
  (slide/title
   "Extended Example"
   (page-para "Expansion of first half:")
   (vl-append
    gap-size
    (code (define-cbr (f a b)
            (swap a b)))
    expands
    cbr-swap-expansion))
  
  (slide/title
   "Extended Example"
   (page-para "Expansion of second half:")
   (vl-append
    gap-size
    (code (let ([x 1] [y 2])
            (f x y)
            x))
    expands
    (code (let ([x 1] [y 2])
            (do-f (lambda () x) 
                  (lambda () y)
                  (lambda (v) (set! x v)) 
                  (lambda (v) (set! y v)))
            x))))
           
  (define define-cbr-defn
    (scode 
     (define-syntax define-cbr
       (syntax-rules ()
	 [(_ (id arg ...) body)
	  (begin
	    (define-for-cbr do-f (arg ...)
	      () body)
	    (define-syntax id
	      (syntax-rules ()
		[(id actual (... ...))
		 (do-f (lambda () actual) 
		       (... ...)
		       (lambda (v) 
			 (set! actual v))
		       (... ...))])))]))))
  
  (slide/title
   "Call-by-Reference Setup"
   (page-para "How the first half triggers the second half:")
   define-cbr-defn)

  (slide/title
   "Call-by-Reference Body"
   (page-para "Remaining expansion to define:")
   (vl-append
    gap-size
    (code (define-for-cbr do-f (a b)
            () (swap a b)))
    expands
    cbr-swap-expansion)
   'next
   (blank)
   (colorize (page-para* "How can" (code define-for-cbr) "make" (code get-) "and" (code put-!) "names?")
             BlueColor))
         
  (define define-for-cbr-defn
   (scode (define-syntax define-for-cbr
            (syntax-rules ()
              [(define-for-cbr do-f (id0 id ...)
                 (gens ...) body)
               (define-for-cbr do-f (id ...) 
                 (gens ... (id0 get put)) body)]
              [(define-for-cbr do-f ()
                 ((id get put) ...) body)
               (define (do-f get ... put ...)
                 (define-get/put-id id get put) ...
                 body)]))))
  
  (slide/title
   "Call-by-Reference Body"
   (page-para "A name-generation trick:")
   define-for-cbr-defn)

  (slide/title
   "Call-by-Reference Body"
   (page-para "More accurate description of the expansion:")
   (vl-append
    gap-size
    (code (define-for-cbr do-f (a b)
            () (swap a b)))
    expands
    (code (define (do-f get^1 get^2 put^1 put^2)
            (define-get/put-id a get^1 put^1)
            (define-get/put-id b get^2 put^2)
            (swap a b)))))
  
  (slide/title
   "Complete Code to Add Call-By-Reference"
   (ht-append
    gap-size
    (vl-append
     gap-size
     (scale define-cbr-defn 0.5 0.5)
     (scale define-for-cbr-defn 0.5 0.5))
    (scale define-get/put-id-defn 0.5 0.5))
   (colorize
    (page-para* "Relies on lexical scope and macro-generating macros")
    BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'lexical)

  (re-slide rename-slide)

  (define roughly-expands
    (vc-append (- line-sep) (colorize (t "~") RedColor) expands))

  (with-steps
   (setup no yes usual)
   (slide/title
    "Reminder: Lexical Scope for Functions"
    (vl-append
     (code (define (app-it f)
	     (let ([x 12])
	       (f x)))
	   code:blank
	   (let ([x 10])
	     (app-it (lambda (y) (+ y x)))))
     ((vafter setup)
      (cc-superimpose (colorize sym:rightarrow BlueColor)
		      ((vbetween no no) (colorize (tt "/") RedColor))))
     (lt-superimpose
      ((vbetween no no)
       (code (let ([x 10])
	       (let ([x 12])
		 ((lambda (y) (+ y x)) x)))))
      ((vafter yes)
       (code (let ([x 10])
	       (let ([_z 12])
		 ((lambda (y) (+ y x)) _z)))))))
    (blank)
    (lt-superimpose
     ((vbetween no no)
      (colorize (page-para* (it "Bad capture")) RedColor))
     ((vafter yes)
      (colorize (page-para* "Ok with" (hbl-append (symbol 97) (t "-rename"))
			    "inside" (code app-it))
		BlueColor)))
    (blank)
    ((vafter usual)
     (page-para "But usual strategy must see the binding..."))))

  (slide/title
   "Bindings in Templates"
   'alts
   (list (list swap-defn
	       (blank)
	       (page-para "Seems obvious that" (code tmp) "can be renamed"))
	 (list
	  (scode
	   (define-syntax swap
	     (syntax-rules ()
	       [(swap a b) (let-one [tmp b]
			     (set! b a)
			     (set! a tmp))])))
	  'next
	  (blank)
	  'alts
	  (list (list
		 (page-item "Rename" (code tmp) "if")
		 (scode (define-syntax let-one
			  (syntax-rules ()
			    [(let-one (x v) body) 
			     (let ([x v]) body)]))))
		(list
		 (page-item (colorize (it "Cannot") RedColor) "rename" (code tmp) "if")
		 (scode (define-syntax let-one
			  (syntax-rules ()
			    [(let-one (x v) body) 
			     (list 'x v body)])))
		 'next
		 (blank)
		 (colorize (page-para "Scheme tracks identifier introductions,"
				      "then renames only as binding forms are discovered")
			   BlueColor))))))
  
  (with-steps
   (hygiene reftrans)
   (slide/title
    "Lexical Scope via Tracking, Roughly"
    swap-defn
    (blank)
    (ct-superimpose
     ((vbetween hygiene hygiene)
      (vc-append
       gap-size
       (page-item "Tracking avoids capture by introduced variables")
       (expands-table
	(code (let ([tmp 5]
		    [other 6])
		(swap tmp other)))
	roughly-expands
	(code (let ([tmp 5]
		    [other 6])
		(let^1 ([tmp^1 other])
		  (set!^1 other tmp)
		  (set!^1 tmp tmp^1)))))
       (page-para* (typeset-code (datum->syntax-object #f (string->symbol "\240^1")))
		   "means introduced by expansion")
       (page-para* (code tmp^1) "does not capture" (code tmp))))
     ((vbetween reftrans reftrans)
      (vc-append
       gap-size
       (page-item "Tracking also avoids capture" (it "of") "introduced variables")
       (expands-table
	(code (let ([set! 5]
		    [let 6])
		(swap set! let)))
	roughly-expands
	(code (let ([set! 5]
		    [let 6])
		(let^1 ([tmp^1 let])
		  (set!^1 let set!)
		  (set!^1 set! tmp^1)))))
       (page-para* (code set!) "does not capture" (code set!^1))
       (page-para* (code let) "does not capture" (code let^1)))))))

  (define precise-title "Precise Rules for Expansion and Binding")

  (define-syntax pscale (syntax-rules () [(_ e) (scale/improve-new-text e 0.9 0.9)]))

  (with-steps
   (let added added-more continue last-name)
   (slide/title
    precise-title
    (page-para
     (htl-append
      gap-size
      (pscale
       (code (let ([tmp 5]
		   [other 6])
	       (swap tmp other))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
	(code (let ([tmp_0 5]
		    [other_0 6])
		(swap tmp_0 other_0)))))))
    ((vafter continue) 
     (page-para
      (htl-append 
       gap-size
       expands
       (pscale
	(code (let ([tmp_0 5]
		    [other_0 6])
		(let^1 ([tmp^1 other_0])
		  (set!^1 other_0 tmp_0)
		  (set!^1 tmp_0 tmp^1)))))
       ((vafter last-name) expands)
       ((vafter last-name)
	(pscale
	 (code (let ([tmp_0 5]
		     [other_0 6])
		 (let^1 ([tmp_2 other_0])
		   (set!^1 other_0 tmp_0)
		   (set!^1 tmp_0 tmp_2)))))))))
    (blank)
    (lt-superimpose
     ((vbetween added added-more)
      (vc-append
       gap-size
       (page-para "When the expander encounters" (code let)
		  ", it renames bindings by adding a subscript")
       ((vbetween added-more added-more)
	(vc-append
	 gap-size
	 (page-para "If a use turns out to be" (hbl-append (code quote) (t "d"))
		    ", the subscript will be erased")
	 (htl-append gap-size
		     (code (let ([x 1])
			     'x))
		     expands
		     (code (let ([x_1 1])
			     'x_1))
		     expands
		     (code (let ([x_1 1])
			     'x)))))))
     ((vbetween continue continue)
      (page-para "Then expansion continues, adding superscripts for introduced identifiers"))
     ((vbetween last-name last-name)
      (page-para "Again, rename for" (code let) (symbol 190)
		 "but only where superscripts match")))))

  ;; >>>>>>>> CUT and PASTE from previous <<<<<<
  ;;  The problem is that it's really difficult to abstract
  ;;  when the source location matters....
  (with-steps
   (let added continue last-name)
   (slide/title
    precise-title
    (page-para
     (htl-append
      gap-size
      (pscale
       (code (let ([set! 5]
		   [let 6])
	       (swap set! let))))
      ((vafter added) expands)
      ((vafter added)
       (pscale
	(code (let ([set!_0 5]
		    [let_0 6])
		(swap set!_0 let_0)))))))
    ((vafter continue) 
     (page-para
      (htl-append 
       gap-size
       expands
       (pscale
	(code (let ([set!_0 5]
		    [let_0 6])
		(let^1 ([tmp^1 let_0])
		  (set!^1 let_0 set!_0)
		  (set!^1 set!_0 tmp^1)))))
       ((vafter last-name) expands)
       ((vafter last-name)
	(pscale
	 (code (let ([set!_0 5]
		     [let_0 6])
		 (let^1 ([tmp_2 let_0])
		   (set!^1 let_0 set!_0)
		   (set!^1 set!_0 tmp_2)))))))))
    (blank)
    (lt-superimpose
     ((vbetween last-name last-name)
      (page-para "Superscript does not count as a rename, so"
		 (code let) "and" (code let^1) "refer to the usual" (code let))))))

  (define (reduce-sequence a multi? . l)
    (cons (list (page-para a)
		'next
		(if multi?
		    (page-para expands expands)
		    (page-para expands))
		(page-para (car l)))
	  (if (null? (cdr l))
	      null
	      (apply reduce-sequence l))))

  (slide/title
   "Local Macros"
   'alts
   (list* (list
	   (page-para
	    (scode (define (run-clock get put!)
		     (define-get/put-id clock get put!)
		     (code:encloud (set! clock (add1 clock))))))
	   'next
	   (page-para expands expands)
	   (page-para
	    (scode (define (run-clock get_0 put!_0)
		     (define-get/put-id clock_1 get_0 put!_0)
		     (code:encloud (set! clock_1 (add1 clock_1))))))
	   'next
	   (page-para expands expands)
	   (page-para
	    (scode (define (run-clock get_0 put!_0)
		     (define-get/put-id clock_1 get_0 put!_0)
		     (code:encloud (put_0^2 (add1 (get_0^3))))))))
	  (reduce-sequence
	   (scode (define (run-clock get put!)
		    (define-get/put-id clock get put!)
		    (code:encloud
		     (let ([get .....])
		       (code:encloud
			(set! clock (get clock)))))))
	   #f
           (scode (define (run-clock get_0 put!_0)
		    (define-get/put-id clock_1 get_0 put!_0)
		    (code:encloud
		     (let ([get_0 .....])
		       (code:encloud
			(set! clock_1 (get_0 clock_1)))))))
	   #f
           (scode (define (run-clock get_0 put!_0)
		    (define-get/put-id clock_1 get_0 put!_0)
		    (code:encloud
		     (let ([get_2 .....])
		       (code:encloud
			(set! clock_1 (get_2 clock_1)))))))
	   #t
	   (scode (define (run-clock get_0 put!_0)
		    (define-get/put-id clock_1 get_0 put!_0)
		    (code:encloud
		     (let ([get_2 .....])
		       (code:encloud
			(put!_0^3 (get_2 (get_0^4)))))))))))
   

  (slide/title/center
   "General Strategy Summarized"
   (page-para "While expanding")
   (page-item "Primitive binding form:" )
   (page-subitem "Change subscript in scope for matching names, subscript, and superscripts")
   (page-item "When looking for binders of a use:")
   (page-subitem "Check for matching name and subscript, only")
   (page-item "After expanding a macro use:")
   (page-subitem "Add a superscript to introduced identifiers")
   (page-para* "(macro-generating macros can stack superscripts)"))
	      

  (slide/title/center
   "Terminology"
   (page-para "Avoid capture" (it "by") "introduced:" (dt "hygiene"))
   (page-para "Avoid capture" (it "of") "introduced:" (dt "referential transparency"))
   (page-para* "Together" sym:implies (dt "lexical scope"))
   (blank)
   (colorize (page-para* "Lexically scoped macros play nice together")
	     BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'transformers)

  (slide/title
   "Transformer Definitions"
   (page-para "In general," (code define-syntax) "binds a transformer procedure")
   (scode (define-syntax swap
	    (lambda (stx)
	      .....)))
   'next
   (blank)
   (page-para "Argument to transformer is a" (dt "syntax object")
	      ": like an S-expression, but with context info"))

  (define (not-really-prim v) v)

  (slide/title
   "Primitives for Transformers"
   (page-para "Primitives deconstruct and construct syntax objects:")
   (vl-append
    (/ gap-size 2)
    (page-para* (not-really-prim (code (stx-car _stx) -> _stx)))
    (page-para* (not-really-prim (code (stx-cdr _stx) -> _stx)))
    (page-para* (not-really-prim (code (stx-pair? _stx) -> _bool)))
    (page-para* (not-really-prim (code (identifier? _stx) -> _bool)))
    (tt " ")
    (page-para* (code (quote-syntax _datum) -> _stx))
    (tt " ")
    (page-para* (code (bound-identifier=? _stx1 _stx2) -> _bool))
    (page-para* (code (free-identifier=? _stx1 _stx2) -> _bool))
    (page-para* (code (datum->syntax-object _stx _v) -> _stx))))


  (slide/title
   "Syntax-Rules as a Transformer"
   (page-para (code syntax-rules) "is actually a macro")
   (vl-append
    gap-size
    (scode (define-syntax swap
	     (syntax-rules .....)))
    expands
    (scode (define-syntax swap
	     (lambda (stx)
	       (code:encloud #,(vc-append
				(page-para* "use transformer primitives to")
				(page-para* (t "match") (code stx)
					    (t "and generate result")))))))))

  (define syntax-case-title "Pattern-Matching Syntax and Having It, Too")

  (slide/title
   syntax-case-title
   (page-para "The" (code syntax-case) "and" (colorize (tt "#'") keyword-color)
	      "forms combine patterns and arbitrary computation")
   (vl-append
    gap-size
    (scode (syntax-case _stx-expr ()
	     [_pattern _result-expr]
	     ...
	     [_pattern _result-expr]))
    (blank)
    (scode #'template))
   'next
   (blank)
   (page-para (code syntax-case) "and" (colorize (tt "#'") keyword-color)
	      "work anywhere")
   (page-subitem "useful for sub-expression matches"))

  (slide/title
   syntax-case-title
   (page-para "Actually," (code syntax-rules) "is implemented in terms of"
	      (code syntax-case))
   (vl-append
    gap-size
    (scode (define-syntax swap
	     (syntax-rules ()
	       [(swap a b) (let ([tmp b])
			     (set! b a)
			     (set! a tmp))])))
    expands
    (scode (define-syntax swap
	     (lambda (stx)
	       (syntax-case stx ()
		 [(swap_1 a b) #'(let ([tmp b])
				   (set! b a)
				   (set! a tmp))]))))))
  
  (slide/title
   "Syntax-Case for a Better Swap Macro"
   (page-para "Check for identifiers before expanding:")
   (scode (define-syntax swap
	    (lambda (stx)
	      (syntax-case stx ()
		[(_ a b) 
		 (if (and (identifier? #'a) 
			  (identifier? #'b))
		     #'(let ([tmp b])
			 (set! b a)
			 (set! a tmp))
		     (raise-syntax-error
		      'swap "needs identifiers" 
		      stx))])))))

  (slide/title
   "Syntax-case for a Better Call-by-Ref Macro"
   (page-para "Use" (code generate-temporaries) "to produce a list ids:")
   (scale/improve-new-text
    (scode 
     (define-syntax (define-for-cbr stx)
       (syntax-case stx ()
	 [(_ id (arg ...) body)
	  (with-syntax ([(get ...)
			 (generate-temporaries #'(arg ...))]
			[(put ...)
			 (generate-temporaries #'(arg ...))])
	    #'(define (do-f get ... put ...)
		(define-get/put-id id get put) ... 
		body))])))
    0.8 0.8))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'art)

  (slide/title
   "Scheme Today"
   (page-item "Standard Scheme (R5RS) provides only" (code syntax-rules))
   (page-item "Most implementations also provide" (code syntax-case))
   (page-subitem "Public expander implementation in R5RS")
   (page-subitem "Syntax-object primitives vary")
   (page-subitem "Separation of compile-time and run-time code varies greatly")
   (page-item "Some implementations support identifier macros")
   'next
   (blank)
   (blank)
   (colorize
    (page-para* "Code in these slides is somewhat specific to PLT Scheme...")
    BlueColor))

  (slide/title/center
   "Slide Language"
   (colorize
    (page-para "... actually, it's PLT Scheme plus")
    BlueColor)
   (blank)
   (scode (define-syntax syntax-id-rules
	    (syntax-rules ()
	      [(_ kws [pat tmpl] ...) 
	       (make-set!-transformer
		(lambda (stx)
		  (syntax-case stx kws
		    [pat #'tmpl]
		    ...)))])))
   (blank)
   (colorize
    (page-para "in a" (code module) "loaded with" (code require-for-syntax))
    BlueColor))

  (slide/title/center
   "Scheme in the Future"
   (colorize
    (page-para* "There's no one Scheme")
    BlueColor)
   'next
   (page-para* "or")
   (colorize
    (page-para* "Scheme is a langauge for defining practical languages")
    BlueColor)
   'next
   (blank)
   (page-item "Standardized language-declaration syntax may be"
	      "the way to tame implementation differences")
   (page-item "In DrX, we intend to push the limits of these ideas"))

  (define (cite who what where)
    (vl-append 
     line-sep
     (page-para* who)
     (page-para* (format "\"~a\"" what))
     (page-para* where)))

  (define (subject s) (bt s))
  (define (maybe p)
    (colorize p "Dark Gray"))

  (slide/title/tall
   "References, Abridged"
   (table 2
	  (list 
	   (subject "hygiene")
	   (cite "Kohlbecker, Friedman, Felleisen, and Duba"
		 "Hygienic Macro Expansion"
		 "LFP 1986")
	   (subject "patterns")
	   (cite "Clinger and Rees"
		 "Macros That Work"
		 "POPL 1991") 
	   (subject "lexical scope")
	   (cite "Dybvig, Hieb, and Bruggeman"
		 "Syntactic Abstraction in Scheme"
		 (hbl-append (it "Lisp and Symbolic Computation") (t " 1993")))
	   (maybe
	    (subject "splicing scope"))
	   (maybe
	    (cite "Waddell and Dybvig"
		  "Extending the Scope of Syntactic Abstraction"
		  "POPL 1999"))
	   (maybe
	    (subject "phases"))
	   (maybe 
	    (cite "Flatt"
		  "Composable and Compilable Macros"
		  "ICFP 2002")))
	  ltl-superimpose ltl-superimpose
	  gap-size (/ gap-size 2)))
  )

