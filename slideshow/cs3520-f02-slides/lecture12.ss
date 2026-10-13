
(module lecture12 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss"
	   "utils/eslide.ss"
	   (lib "list.ss"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define nbsp (string (integer->char 160)))

  (define (prog-pre1 p  var arg)
    (alg-code$* 
     p
     (format "let f = proc(~ax) set x = 1" var)
     "   in let y = 0"
     (format "       in { (f ~a);" arg)
     (format "            y~a}" nbsp)))


  (define (prog1 p) 
    (prog-pre1 p "" "y"))

  (eslide-title "Assigning to a Variable")

  (slide/title
   (eslide-title)
   (page-para "What is the result of this program?")
   (blank)
   (prog1 "!")
   (blank)
   (page-para "Is it 0 or 1?"))

  (eslide (point empty-env)
	  (prog1 #f))

  (define (mk-f conly?) 
    (closure-env conly? 0 
		 (env-pict empty-env) (env-frame empty-env)
		 (env-frame empty-env) 
		 "f" "x" "set x = 1" #f))

  (define pre-f-env (mk-f #t))
  (eslide (point (env-pict pre-f-env) (env-frame empty-env))
	  (prog1 "proc.*"))

  (define f-env (mk-f #f))
  (eslide (point (env-pict f-env))
	  (prog1 "(let y.*)|(in { .*)|(y.})"))
  
  (define y-env (env-extension '("y") '("0") #f f-env))
  (eslide (point y-env)
	  (prog1 "(.f y.)"))

  (define x-env (add-binding font-size #t "x" "0" #f (env-pict y-env) (env-frame empty-env)))
  (eslide (point x-env)
	  (prog1 "set x = 1"))

  (define x2-env (add-binding font-size #t "x" (colorize (alg-code "1") red) #f (env-pict y-env) (env-frame empty-env)))
  (eslide (point x2-env)
	  (prog1 "set x = 1"))

  (define x3-env (add-binding font-size #t "x" "1" #f (env-pict y-env) (env-frame empty-env)))
  (eslide (point (env-pict x3-env) (env-frame y-env))
	  (prog1 (format "y~a" nbsp))
	  "So the answer is 0")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (C++-vers var? deref?)
    (format "
void f(int~a x) {
  ~ax = 1;
}
 
int main() {
  int y = 0;
  f(~ay);
  return y;
}"
	    (if var? "&" (if deref? "*" ""))
	    (if deref? "*" "")
	    (if deref? "&" "")))

  (slide/title
   "Variables in C++"
   (blank)
   (prog #f (C++-vers #f #f))
   (blank)
   (page-para "The result above is 0, too"))

  (slide/title
   "Variables in C++"
   (blank)
   (prog "&" (C++-vers #t #f) red)
   (blank)
   'alts
   (list (list
	  (page-para "But the result above is 1"))
	 (list
	  (page-para "This example shows" (dt "call-by-reference") ".")
	  (page-para "The previous example showed" (dt "call-by-value") "."))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eslide-title "Assignment and Call-by-Reference")

  (define (prog2 p) (prog-pre1 p "&" "y"))
  
  (eslide (point empty-env)
	  (prog-pre1 "&" "&" "y")
	  "Adding call-by-reference parameters to our language")

  (define p2-refs? #f)

  (define (mk-f2 conly?) (closure-env conly? 0 
				     (env-pict empty-env) (env-frame empty-env)
				     (env-frame empty-env)
				     "f" "&x" "set x = 1" p2-refs?))

  (define pre-f2-env (mk-f2 #t))
  (eslide (point (env-pict pre-f2-env) (env-frame empty-env))
	  (prog2 "proc.*"))

  (define f2-env (mk-f2 #f))
  (eslide (point f2-env)
	  (prog2 "(let y.*)|(in {.*)|(y.})"))

  (define (mk-y v f2-env) (env-extension '("y") (list v) p2-refs? f2-env))
  (define y2-env (mk-y "0" f2-env))
  (eslide (point y2-env)
	  (prog2 "(.f y.)"))

  (define (mk-x v) (let ([y-env (mk-y v f2-env)]
			 [x-bullet (colorize (launder bullet) BlueColor)])
		     (let ([x-env (add-binding font-size #t "x" x-bullet #f (env-pict y-env) (env-frame empty-env))])
		       (values
			(make-env
			 (cc-superimpose
			  (env-pict x-env)
			  (colorize
			   (cons-picture
			    (ghost (env-pict x-env))
			    (let-values ([(yr yc) (find-rc (env-pict x-env) (env-frame y-env))]
					 [(xr xc) (find-rc (env-pict x-env) x-bullet)]
					 [(?) (tt "?")]
					 [(arrow-size) (/ font-size 2)])
			      `((place ,(+ yr arrow-size) ,yc ,(arrow-line (- arrow-size) 0 arrow-size))
				(curve ,xr ,xc ,(+ xr font-size) ,(/ (+ xc yc) 2) ,(+ xr font-size) ,xc)
				(curve ,yr ,yc ,(+ xr font-size) ,(/ (+ xc yc) 2) ,(+ xr font-size) ,yc)
				(place ,(+ xr font-size) ,(/ (- (+ xc yc) (pict-height ?)) 2) ,?))))
			   BlueColor))
			 (env-frame x-env)
			 (list #f))
			(env-frame y-env)))))
  (define-values (x21-env y/x-frame) (mk-x "0"))
  (eslide (point x21-env)
	  (prog2 "set x = 1")
	  "The pointer from one environment frame to another is questionable,"
	  "because frames are supposed to point to values")

  (define-values (x22-env y/x2-frame) (mk-x (colorize (alg-code "1") red)))
  (eslide (point x22-env)
	  (prog2 "set x = 1"))
  
  (define-values (x23-env y/x23-frame) (mk-x "1"))
  (eslide (point (env-pict x23-env) y/x23-frame)
	  (prog2 (format "y~a" nbsp)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Interpreter Changes"
   (page-para "Same as before:")
   (page-item "Expressed values: Number + Proc")
   (page-item "Denoted values: Ref(Expressed Value)")
   (blank)
   (page-para "The difference is that application doesn't always "
	      "create a new location for a new variable binding")
   'next
   (blank)
   (page-para "=> Separate" (colorize (it "location") RedColor)
	      "creation from" (colorize (it "environment") BlueColor) 
	      "extension"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (small-ex refs?) (env-extension '("x" "y") '("10" "12") refs? empty-env))

  (define (small-prog  p)
    (alg-code$* p
		"let x = 10"
		"    y = 12"
		"  in +(x,y)"))
    
  (eslide (vl-append font-size
		     (point (small-ex #f)))
	  (small-prog "[+].*")
	  "The old way")
  (eslide (vl-append font-size
		     (point (small-ex #t)))
	  (small-prog "[+].*")
	  "The new way")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eslide-title "Call-by-Reference")

  (eslide (point empty-env)
	  (prog2 #f)
	  "Do the previous evaluation the new way...")

  (set! p2-refs? #t)

  (define pre-f3-env (mk-f2 #t))
  (eslide (point (env-pict pre-f3-env) (env-frame empty-env))
	  (prog2 "proc.*"))

  (define f3-env (mk-f2 #f))
  (eslide (point f3-env)
	  (prog2 "(let y.*)|(in {.*)|(y.})"))
  
  (define y3-env (mk-y "0" f3-env))
  (eslide (point y3-env)
	  (prog2 "(.f y.)"))

  (define (mk-x3 v) (let ([y-env (mk-y v f3-env)]
			  [x-bullet (colorize (launder bullet) RedColor)]
			  [arrow-size (/ font-size 2)])
		      (let ([x-env (add-binding font-size #t "x" x-bullet #f (env-pict y-env) (env-frame empty-env))])
			(values
			 (make-env
			  (cc-superimpose
			   (env-pict x-env)
			   (colorize
			    (cons-picture
			     (ghost (env-pict x-env))
			     (let-values ([(yr yc) (find-rc (env-pict x-env) (car (env-refs y-env)))]
					  [(xr xc) (find-rc (env-pict x-env) x-bullet)]
					  [(fs) (* 2 font-size)])
			       `((place ,(+ yr arrow-size) ,yc ,(arrow-line (- arrow-size) 0 arrow-size))
				 (curve ,xr ,xc ,(+ xr fs) ,(/ (+ xc yc) 2) ,(+ xr fs) ,xc)
				 (curve ,yr ,yc ,(+ xr fs) ,(/ (+ xc yc) 2) ,(+ xr fs) ,yc))))
			    RedColor))
			  (env-frame x-env)
			  (env-refs y-env))
			 (env-frame y-env)))))
  (define-values (x30-env y/x30-frame) (mk-x3 "0"))
  (eslide (point x30-env)
	  (prog2 "set x = 1")
	  "This time, the new environment frame points to a"
	  "location box, which is consistent with other frames")

  (define-values (x32-env y/x32-frame) (mk-x3 (colorize (alg-code "1") red)))
  (eslide (point x32-env)
	  (prog2 "set x = 1"))

  (define-values (x33-env y/x33-frame) (mk-x3 "1"))
  (eslide (point (env-pict x33-env) y/x33-frame)
	  (prog2 (format "y~a" nbsp)))

  (eslide-title "Call-by-Reference with Non-variables")

  (define (prog2.5 p) (prog-pre1 p  "&" "0"))
  
  (eslide (point y3-env)
	  (prog2.5 ".f 0.")
	  "If call-by-reference argument is not a variable...")

  (define x34-env (add-binding font-size #t "x" (colorize (alg-code "0") GreenColor) #t (env-pict y3-env) (env-frame empty-env)))
  (eslide (point x34-env)
	  (prog2.5 "set x = 1")
	  "... create a location")


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Interpreter Changes"
   (page-item "Add call-by-reference arguments (indicated by" (hbl-append (alg-code "&") (t ")")))
   (page-item "New" (alg-code "var") "datatype, with" (tt "cbv-var") "and" (tt "cbr-var") "variants")
   (page-item "Create explicit" (colorize (t "locations") RedColor) "for variables")
   (vl-append
    line-sep
    (tt "location : expval -> location")
    (tt "location-val : location -> expval")
    (tt "location-set! : location expval -> void"))
   (page-item "Change variable lookup to de-reference locations")
   (page-item "Change" (alg-code "set") "to work on locations")
   (page-item "Add" (tt "eval-fun-rands") "and change" (tt "apply-proc")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "& versus * in C++"
   (prog "&|[*]" (C++-vers #f #t) red)
   'next
   (blank)
   (page-item "This is back to" (dt "call-by-value") ", but "
	      "with a reference as a value")
   (blank)
   (page-item "To study this form of call, we can add explicit "
	      "references to our language, too"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eslide-title "Call-by-Value with References")

  (define (prog3 p)
    (alg-code$* p 
		"  let f = proc(x) setref(x, 1)"
		"   in let y = 0"
		"       in { (f ref(y));"
		(format "            y~a}" nbsp)))
	  
  (define (mk-f4 conly?) (closure-env conly? 0 
				      (env-pict empty-env) (env-frame empty-env)
				      (env-frame empty-env)
				      "f" "x" "setref(x, 1)" #t))
  (define pre-f4-env (mk-f4 #t))
  (eslide (point (env-pict pre-f4-env) empty-env)
	  (prog3 "proc.*"))
  
  (define f4-env (mk-f4 #f))
  (eslide (point f4-env)
	  (prog3 "(let y.*)|(in {.*)|(y.})"))

  (define (mk-y4 v cell?) (let ([y-env (env-extension '("y") (list v) #t f4-env)]
				[arrow-size (/ font-size 2)])
			    (if cell?
				(let* ([cell-bullet (colorize (launder bullet) RedColor)]
				       [cell (env-array GreenColor cell-bullet)]
				       [ref (car (env-refs y-env))])
				  (values
				   (make-env
				    (cc-superimpose
				     (env-pict y-env)
				     (let-values ([(rr rb) (find-rb (env-pict y-env) ref)]
						  [(db _) (find-lc cell cell-bullet)])
				       (cons-picture
					(colorize 
					 (cons-picture
					  (ghost (env-pict y-env))
					  (let ([len (+ font-size db)])
					    `((place ,(+ rr len) ,(+ rb (/ (pict-height ref) 2))
						     ,(arrow-line (- len) 0 arrow-size)))))
					 RedColor)
					`((place ,(+ rr font-size) ,rb ,cell)))))
				    (env-frame y-env)
				    (env-refs y-env))
				   cell))
				y-env)))
	 
  (define y4-env (mk-y4 "0" #f))
  (eslide (point y4-env)
	  (prog3 "(.f re.*)"))

  (eslide (point y4-env)
	  (prog3 "ref.y."))

  (define-values (y42-env _) (mk-y4 "0" #t))
  (eslide (point y42-env)
	  (prog3 "ref.y."))

  (define (mk-x4 v)
    (let*-values ([(y-env y-ref) (mk-y4 v #t)]
		  [(x-bullet) (colorize (launder bullet) GreenColor)]
		  [(arrow-size) (/ font-size 2)])
      (let ([x-env (add-binding (* 4 font-size) #t "x" x-bullet #t (env-pict y-env) (env-frame empty-env))])
	(values
	 (make-env
	  (cc-superimpose
	   (env-pict x-env)
	   (colorize
	    (cons-picture
	     (ghost (env-pict x-env))
	     (let-values ([(yr yc) (find-rc (env-pict x-env) y-ref)]
			  [(xr xc) (find-rc (env-pict x-env) (car (env-refs x-env)))])
	       `((place ,(+ yr arrow-size) ,yc ,(arrow-line (- arrow-size) 0 arrow-size))
		 (curve ,xr ,xc ,(+ xr font-size) ,(/ (+ xc yc) 2) ,(+ xr font-size) ,xc)
		 (curve ,yr ,yc ,(+ xr font-size) ,(/ (+ xc yc) 2) ,(+ xr font-size) ,yc))))
	    GreenColor))
	  (env-frame x-env)
	  (list #f))
	 (env-frame y-env)))))

  (define-values (x4-env y/x4-frame) (mk-x4 "0"))
  (eslide (point x4-env)
	  (prog3 "setref.x, 1."))
  
  (define-values (x42-env y/x42-frame) (mk-x4 (colorize (alg-code "1") red)))
  (eslide (point x42-env)
	  (prog3 "setref.x, 1."))
  
  (define-values (x43-env y/x43-frame) (mk-x4 "1"))
  (eslide (point (env-pict x43-env) y/x43-frame)
	  (prog3 (format "y~a" nbsp)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Interpreter Changes for References"
   (page-para "Revised language:")
   (page-item "Expressed vals: Number + Proc + Ref(Expressed Val)")
   (page-item "Denoted vals: Ref(Expressed Val)")
   (blank)
   (page-para "Interpreter changes:")
   (page-item "Add" (colorize (t "reference") GreenColor) "values")
   (page-item "Add" (tt "ref") "form and" (tt "setref") "primitive"))

  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title 
   "Lazy Evaluation of Function Arguments"
   (alg-code* 
    "let f = proc(x)0"
    "  in (f +(1,+(2,+(3,+(4,+(5,6))))))")
   'next
   (blank)
   (page-para "The computed" (alg-code "21") "is never used.")
   'next
   (blank)
   (page-para "What if we were" (it "lazy") "about computing function"
	      "arguments (in case they aren't used)?"))

  (slide/title 
   "Lazy Evaluation of Function Arguments"
   (page-para "One way to laziness:")
   (alg-code*
    "let f = proc(xthunk)0"
    "  in (f proc()+(1,+(2,+(3,+(4,+(5,6))))))")
   'next
   (blank)
   (alg-code*
    "let f = proc(xthunk)-((xthunk), 7)"
    "  in (f proc()+(1,+(2,+(3,+(4,+(5,6))))))")
   'next
   (blank)
   (page-para "By using" (alg-code "proc") "to delay evaluation,"
	      "we can avoid unnecessary computation.")
   'next
   (blank)
   (page-para "How about making the language compute function arguments"
	      "lazily in" (it "all") "applications?"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eslide-title "Evaluation with Lazy Arguments")

  (define (prog-pre p body)
    (alg-code$*
     p
     (format "let f = proc(x)~a" body)
     "  in (f +(1,2))"))

  (define (lprog1 p) (prog-pre p "0"))

  (eslide (point empty-env)
	  (lprog1 #f))

  (define lf-env (closure-env #f 0 
			     (env-pict empty-env) (env-frame empty-env) 
			     (env-frame empty-env)
			     "f" "x" "0" #t))
  (eslide (point lf-env)
	  (lprog1 ".f [+].*"))

  (define (lmk-x conly?) (closure-env conly? 2 
				     (env-pict lf-env) (env-frame empty-env)
				     (env-frame lf-env) "x" #f "+(1,2)" #t))
  (eslide (point (env-pict (lmk-x #t)) (env-frame lf-env))
	  (lprog1 ".f [+].*")
	  "Application creates a new kind of green box, with two slots: a"
	  (dt "thunk"))

  (define lx-env (lmk-x #f))
  (eslide (point lx-env)
	  (lprog1 "0"))

  (eslide (no-point lx-env)
	  (lprog1 #f)
	  "The result is 0")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (lprog2 p) (prog-pre p "-(x,1)"))

  (eslide (point empty-env)
	  (lprog2 #f))

  (define lf2-env (closure-env #f 0 
			      (env-pict empty-env) (env-frame empty-env) 
			      (env-frame empty-env)
			      "f" "x" "-(x,1)" #t))
  (eslide (point lf2-env)
	  (lprog2 ".f [+].*"))

  (define (lmk-x2 conly?) (closure-env conly? 2 
				      (env-pict lf2-env) (env-frame empty-env)
				      (env-frame lf2-env) 
				      "x" #f "+(1,2)" #t))
  (eslide (point (env-pict (lmk-x2 #t)) (env-frame lf2-env))
	  (lprog2 ".f [+].*"))

  (define lx2-env (lmk-x2 #f))
  (eslide (point lx2-env)
	  (lprog2 "[-].x,1.")
	  "lookup of" (alg-code "x") "...")

  (eslide (point (env-pict lx2-env) (env-frame lf2-env))
	  (lprog2 "[+].1,2.")
	  "... forces evaluation of the thunk")

  (eslide (point lx2-env)
	  (lprog2 "[-].x,1.")
	  "so 3 is the value of" (alg-code "x"))

  (eslide (no-point lx2-env)
	  (lprog2 #f)
	  "The result is 2")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (lprog3-base simple? p)
    (alg-code$* 
     p
     "let f = proc(x)-(x,1)"
     (format "  in let y = ~a"  (if simple? "7" "+(3,4)"))
     "      in (f +(1,y))"))

  (define (lprog3.0 p) (lprog3-base #t p))
  (define (lprog3 p) (lprog3-base #f p))

  (eslide (point empty-env)
	  (lprog3.0 #f)
	  "Lazy expression that needs its environment...")

  (define lf3-env (closure-env #f 0 
			      (env-pict empty-env) (env-frame empty-env) 
			      (env-frame empty-env)
			      "f" "x" "-(x,1)" #t))
  (eslide (point lf3-env)
	  (lprog3.0 "in.*"))

  (define ly-env (env-extension '("y") '("7") #t lf3-env))
  (eslide (point ly-env)
	  (lprog3.0 ".f [+].*"))
  
  (define (lmk-x3 conly?) (closure-env conly? 2 
				      (env-pict ly-env) (env-frame empty-env)
				      (env-frame ly-env)
				      "x" #f "+(1,y)" #t))
  (eslide (point (env-pict (lmk-x3 #t)) (env-frame ly-env))
	  (lprog3.0 ".f [+].*"))
  
  (define lx3-env (lmk-x3 #f))
  (eslide (point lx3-env)
	  (lprog3.0 "[-].x,1.")
	  "Evaluation of" (alg-code "x") "forces the thunk...")

  (eslide (point (env-pict lx3-env) (env-frame ly-env))
	  (lprog3.0 "[+].1,y.")
	  "Triggering evaluation with the"
	  (hbl-append (it "thunk") (t "'s"))
	  "environment, not the current one")

  (eslide (point (env-pict lx3-env) (env-frame ly-env))
	  (lprog3.0 "[+].1,y.")
	  "(The result will be 7)")

  ;; ;;;;;;;;;;;;;;;;;;;;

  (eslide (point empty-env)
	  (lprog3 #f)
	  "What if the right-hand side for" (alg-code "y") "is an expression, instead of a value?")

  (define lf4-env (closure-env #f 0
			      (env-pict empty-env) (env-frame empty-env) 
			      (env-frame empty-env)
			      "f" "x" "-(x,1)" #t))
  (eslide (point lf4-env)
	  (lprog3 "in.*"))

  (define ly4-env (closure-env #f 0 
			      (env-pict lf4-env) (env-frame lf4-env)
			      (env-frame lf4-env) 
			      "y" #f "+(3,4)" #t))
  (eslide (point ly4-env)
	  (lprog3 ".f [+].*")
	  "Added thunk for the value of" (alg-code "y"))

  (define (lmk-x4 conly?) (closure-env conly? 2
				      (env-pict ly4-env) (env-frame empty-env)
				      (env-frame ly4-env)
				      "x" #f "+(1,y)" #t))
  (eslide (point (env-pict (lmk-x4 #t)) (env-frame ly4-env))
	  (lprog3 ".f [+].*")
	  "Another thunk for the argument of" (alg-code "f"))

  (define lx4-env (lmk-x4 #f))
  (eslide (point lx4-env)
	  (lprog3 "[-].x,1.")
	  "Evaluation of" (alg-code "x") "forces a thunk...")

  (eslide (point (env-pict lx4-env) (env-frame ly4-env))
	  (lprog3 "[+].1,y.")
	  "which, in turn, forces another thunk...")

  (eslide (point (env-pict lx4-env) (env-frame lf4-env))
	  (lprog3 "[+].3,4.")
	  "and so on (to get 7)")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Implementing Lazy Evaluation"
   (page-para "Interpreter changes:")
   (page-item "Change" (tt "eval-fun-rands") "to create thunks")
   (page-item "Change variable lookup to force thunk evaluation")
   (blank)
   (page-para* "(Implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Call-by-Name and Call-by-Need"
   (blank)
   (page-para "The lazy strategy we just implemented is" (dt "call-by-name"))
   'next
   (page-item "Advantage: unneeded arguments are not computed")
   (page-item "Disadvantage: needed arguments may be computed many times")
   (alg-code*
    "let f = proc(x)+(x,+(x,x))"
    "  in (f +(1,+(2,+(3,+(4,+(5,6))))))")
   'next
   (blank)
   (page-para "Best of both worlds:" (dt "call-by-need"))
   (page-item "Evaluates each lazy expression once, then remembers the result"))

  (define (prog2n p) (prog-pre p "-(x,x)"))

  (define f2n-env (closure-env #f 0
			       (env-pict empty-env) (env-frame empty-env) 
			       (env-frame empty-env)
			       "f" "x" "-(x,x)" #t))

  (define (lmk-x2n conly?) (closure-env conly? 2 
				      (env-pict f2n-env) (env-frame empty-env)
				      (env-frame f2n-env) 
				      "x" #f "+(1,2)" #t))

  (define x2n-env (lmk-x2n #f))

  (eslide (point empty-env)
	  (prog2n #f)
	  "Start as before...")
  (eslide (point f2n-env)
	  (prog2n ".f [+].*"))
  (eslide (point (env-pict (lmk-x2n #t)) (env-frame f2n-env))
	  (prog2n ".f [+].*"))
  (eslide (point x2n-env)
	  (prog2n "[-].x,x.")
	  "lookup of" (alg-code "x") "...")
  (eslide (point (env-pict x2n-env) (env-frame f2n-env))
	  (prog2n "[+].1,2.")
	  "... forces evaluation of the thunk to get 3")

  (define x2nn-env (add-binding (* 2 font-size) #t
				"x" (colorize (alg-code "3") GreenColor) #t
				(env-pict f2n-env) (env-frame empty-env)))
  
  (eslide (point x2nn-env)
	  (prog2n "[-].x,x.")
	  "so change" (alg-code "x") "to 3 --- which is the essence of call-by-need")
  (eslide (point x2nn-env)
	  (prog2n "[-].x,x.")
	  "lookup of" (alg-code "x") "again gets 3")

  (eslide (no-point x2nn-env)
	  (prog2 #f)
	  "(The result is 0)")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Implementing Call-by-Need"
   (page-para "Interpreter changes:")
   (page-item "Change variable lookup to replace thunks in locations with their values")
   (blank)
   (page-para* "(Implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Calling Convention Terminology"
   (page-item "Call-by-name and call-by-need =" (dt "lazy") "evaluation")
   (page-item "Call-by-value =" (dt "eager") "evaluation")
   (blank)
   (page-para "Call-by-reference can augment either...")
   (page-para "... but the combination of reference and laziness is difficult to reason about"))

  (slide/title
   "Popular Calling-Convention Choices"
   (page-item "Most languages are call-by-value")
   (page-subitem "C, C++, Pascal, Scheme, Java, ML, Smalltalk...")
   (page-item "Some provide call-by-reference")
   (page-subitem "C++, Pascal")
   (page-item "A few are call-by-need")
   (page-subitem "Haskell")
   (page-item "Practically no languages are call-by-name"))

  (slide/title
   "Popularity of Laziness"
   (page-para "Why don't more languages provide lazy evaluation?")
   'next
   (page-item "Disadvantage: evaluation order is not obvious")
   (alg-code*
    "let x = 0  f = ..."
    "  in let y = set x=1"
    "           z = set x=2"
    "       in { (f y z) ; x }"))
 
  (slide/title
   "Popularity of Laziness"
   (page-para "Why do some languages provide lazy evaluation?")
   'next
   (page-item "Evaluation order does not matter if the language has no" 
	      (alg-code "set") "form")
   (page-item "Such languages are called" (dt "purely functional"))
   (page-subitem "Note: call-by-reference is meaningless in a purely functional language")
   (page-item "A language with" (alg-code "set") " can be called " (dt "imperative")))

  (slide/title
   "Laziness and Eagerness"
   (page-para "Even in a purely functional language,"
	      "lazy and eager evaluation can produce different results")
   (blank)
   (alg-code*
    "let f = proc(x)0"
    "  in (f <loopforever>)")
   (blank)
   (page-item "Eager answer: none")
   (page-item "Lazy answer:"  (alg-code "0")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Summary"
   (page-item "Call-by-reference")
   (page-subitem "split location from environment representation")
   (page-subitem "handle function arguments/variables specially")
   (page-item "Call-by-name")
   (page-subitem "thunk all argument expressions")
   (page-subitem "modify variable lookup to evaluate thunks")
   (page-item "Call-by-need")
   (page-subitem "revise variable lookup to install computed thunk result"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
