
(module lecture11 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
	   "utils/env.ss"
	   "utils/eslide.ss"
	   (lib "list.ss"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Assignment in Scheme"
   'alts
   (list
    (list
     (page-para "So far, we have one form of assignment:" (alg-code "vector-set!"))
     (alg-code* "(let ([v (vector 1 2 3)])"
		"  (begin"
		"     (vector-set! v 1 72)"
		"     v))"
		"->>"
		"#(1 72 3)"))
    (list
     (page-para "Scheme actually allows variables to be modified:")
     (alg-code* "(let ([x 2])"
		"   (begin"
		"     (set! x 73)"
		"     x))"
		"->>"
		"73")
     'next
     (blank)
     (page-item (it "Don't write Scheme code like that")
		", except for HW6")
     'next
     (page-item "But many languages have assignment, and need it"))))

  (slide/title
   "Assignment in the Book Language"
   (page-item "Add a" (alg-code "set") "expression form:")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "set id = expr") (blank))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-xy-env x-val color)
    (env-extension '("x" "y") `(,x-val "12") #f empty-env))

  (define (make-xy-env* x-val)
    (let ([e (make-xy-env x-val blue)])
      (values (env-pict e) (env-frame e))))

  (define (make-f-env* conly? x-val color)
    (let-values ([(xy-env xy-frame) (make-xy-env* x-val)])
      (let ([e (closure-env conly? 0
			    xy-env xy-env
			    xy-env
			    "f"
			    "z"
			    "+(z,x)"
			    #f)])
	(values (env-pict e) (env-frame e) xy-frame))))

  (define (make-f-env conly? x-val color)
    (let-values ([(e ef f) (make-f-env* conly? x-val color)])
      e))
  
  (define (make-f2-env* conly? color)
    (let ([e (closure-env conly? 0
			  (env-pict empty-env) (env-frame empty-env)
			  (env-pict empty-env)
			  "f"
			  "z"
			  "let x = 10 in let d = set x = +(x,z) in x"
			  #f)])
      (values (env-pict e) (env-frame e))))

  (define (make-f2-env conly? color)
    (let-values ([(e f) (make-f2-env* conly? color)])
      e))
  
  (define (mk-mk conly? color)
    (let ([e (closure-env conly? 0
			  (env-pict empty-env) (env-frame empty-env)
			  (env-pict empty-env)
			  "mk"
			  "x"
			  "proc(z)let d = set x = +(x,z) in x"
			  #f)])
      (values (env-pict e) (env-frame e))))

  (define (mk-mk/ conly? color)
    (let-values ([(e f) (mk-mk conly? color)])
      e))

  (define-values (mk-env mk-frame) (mk-mk #f blue))

  (define (vt x) (if (string? x)
		     (colorize (alg-code x) GreenColor)
		     x))

  (define (add-binding* extra-indent vert? name val ref? base-env-pict var-frame)
    (let ([e (add-binding (+ extra-indent font-size) vert? name (vt val) ref? base-env-pict var-frame)])
      (values (env-pict e) (env-frame e))))

  (define (made-env conly? name xval base-env-pict base-frame color)
    (let ([x (add-binding (* 2 font-size) #t "x" (vt xval) #f base-env-pict (env-pict empty-env))])
      (let ([e (closure-env conly? 1
			    (env-pict x) base-frame
			    (env-frame x)
			    name
			    "z"
			    "let d = set x = +(x,z) in x"
			    #f)])
	(values (env-pict e) (env-frame e)))))

  (define-values (made-f-env made-f-frame) (made-env #f "f" "10" mk-env mk-frame blue))
  (define-values (made-g-env made-g-frame) (made-env #f "g" "12" made-f-env made-f-frame blue))

  (eslide-title "Evaluating with Assignment")

  (define program1a
    (alg-code$* "!" 
		"let x = 10"
		"    y = 12"
		"  in (begin set x = +(x,1)"
		"        x)"))

  (if condense?
      (skip-slides 1)
      (eslide (no-point (blank)) program1a))
  (eslide (no-point (blank)) program1a 
	  "Can't write this, since we don't have"
	  (alg-code "begin") "in our language")

  (define (program1 active)
    (alg-code$* active 
		"let x = 10"
		"    y = 12"
		"  in let d = set x = +(x,1)"
		"       in x"))

  (eslide (no-point (blank)) (program1 "!")
	  "Instead, use a binding for a dummy variable"
	  (pt "d") "to sequence expressions; initial environment is empty")
  (eslide (point empty-env) (program1 "(10|12)")
	  "Eval RHS (right-hand side) of the let expression")
  (eslide (point (make-xy-env "10" blue)) (program1 "(let d.*|in x)")
	  "Extend the current environment with" (pt "x") "and" (pt* "y")
	  ", and eval body")
  (eslide (point (make-xy-env "10" blue)) (program1 "set x = ..x,1.")
	  "Eval RHS of the let expression")
  (eslide (point (make-xy-env (colorize (alg-code "11") red) blue)) (program1 "set x = ..x,1.")
	  "It modifies the" (pt "x") " in the current lexical scope;"
	  "we define" (pt "set") "to always return" (pt* "1"))
  (define final-env (env-extension '("d") '("1") #f (make-xy-env "11" blue)))
  (eslide (point final-env)
	  (program1 "x$")
	  "Bind" (pt "d") "to the result" (pt* "1")
	  "; to eval the body," (pt* "x") ", we look it up in the environment"
	  "as usual, and find" (pt* "11"))
  (eslide (no-point final-env) (program1 "!")
	  (key-point
	   half-page-para
	   "Variables now correspond to boxes in the environment, not fixed values"))

  (slide/title
   "Expressed and Denoted Values"
   (blank)
   (grammar-table
    (list
     (alg-code "expval") eqls (alg-code "num") (blank)
     (blank)             eqls (alg-code "Proc") (blank)
     (alg-code "denval") eqls (alg-code "reference") (blank)))
   (blank)
   (page-item "New datatype:")
   (prog "
(define-datatype reference reference?
  (a-ref (pos integer?)
	 (vec vector?)))")
   (page-item "New function:")
   (tt "apply-env-ref : env sym -> ref"))

  (eslide-title "Assignment and Closures")
  
  (define (program2 active)
    (alg-code$* active 
		"let x = 10"
		"    y = 12"
		"  in let f = proc(z)+(z,x)"
		"      in let d = set x = +(x,1)"
		"          in (f 0)"))

  (eslide (point empty-env) (program2 "!")
	  "An example with" (pt* "proc") "; again, we start with the"
	  "empty environment")
  (eslide (point empty-env) (program2 "(10|12)")
	  "Eval RHS of the let expression")
  (eslide (point (make-xy-env "10" blue)) (program2 "(let f.*|in let d.*|in .f 0.)")
	  "Extend the current environment with" (pt "x") "and" (pt* "y")
	  ", and eval body")
  (eslide (point (make-xy-env "10" blue)) (program2 "proc.*$")
	  "Eval RHS of the let expression...")
  (define-values (f-env2 f-env2-frame xy-frame) (make-f-env* #t "10" blue))
  (eslide (point f-env2 xy-frame) (program2 "proc.*$")
	  "... which creates a closure, pointing to the current environment")
  (define-values (f-env2f f-env2f-frame xy-framef) (make-f-env* #f "10" blue))
  (eslide (point f-env2f f-env2f-frame) (program2 "(let d.*|in .f 0.)")
	  "To finish the" (pt* "let") ", the environment is extended with"
	  (pt "f") "bound to the closure; then evaluate the body")
  (eslide (point f-env2f f-env2f-frame) (program2 "set x = ..x,1.")
	  "Eval RHS of the let expression...")
  (define-values (f-env2x f-env2x-frame xy-frame2x) (make-f-env* #f (colorize (alg-code "11") red) blue))
  (eslide (point f-env2x f-env2x-frame) (program2 "set x = ..x,1.")
	  "... which changes the value of" (hbl-append (pt* "x") (t ",")) "then produces" (pt* "1"))
  (define-values (f-env3 f-env3-frame xy-frame3) (make-f-env* #f "11" blue))
  (define-values (d-env d-frame) (let ([e (env-extension '("d") '("1") #f f-env3 f-env3-frame)])
				   (values (env-pict e) (env-frame e))))
  (eslide (point d-env) (program2 ".f 0.")
	  "To eval the body," (pt* "(f 0)") ", we look up" (pt "f") "in the environment"
	  "to find a closure, and evaluate" (pt "0") "to" (pt* "0"))
  (define-values (dz-env dz-frame) (add-binding* 0 #t "z" "0" #f d-env xy-frame3))
  (eslide (point dz-env dz-frame) (program2 "..z,x.")
	  "Extend the" (it "closure's") "environment with" (pt "0") "for"
	  (pt* "z") ", and evaluate the closure's body in that environment;"
	  "the result will be" (pt* "11"))
  (eslide (no-point dz-env) (program2 "!")
	  (key-point
	   half-page-para
	   "By capturing environments, closures capture variables that may change"))

  (eslide-title "Assignment and Arguments")

  (define (program2.5 active)
    (alg-code$* active 
		"let f = proc(z)"
		"             let x = 10"
		"              in let d = set x = +(x,z)"
		"                  in x"
		" in +((f 1), (f 9))"))

  (eslide (point empty-env) (program2.5 "!")
	  "Another example with" (pt* "proc") ", but with the" (pt "let")
	  "inside the" (pt* "proc"))
  (eslide (point empty-env) (program2.5 "(proc.*|let x.*|in let.*|in x)")
	  "Eval RHS of the let expression...")
  (eslide (point (make-f2-env #t blue) empty-env) (program2.5 "(proc.*|let x.*|in let.*|in x)")
	  "... which creates a closure, pointing to the current environment")
  (define f2-env (make-f2-env #f blue))
  (eslide (point f2-env) (program2.5 "[+]..f.*")
	  "Bind the closure to" (pt "f") "and eval the body")
  (eslide (point f2-env) (program2.5 ".f 1.")
	  "Evaluate the first operand," (pt* "(f 1)"))
  (define-values (z-there z-there-frame) (add-binding* 0 #t "z" "1" #f f2-env (env-frame empty-env)))
  (eslide (point z-there z-there-frame) (program2.5 "(let x.*|in let.*|in x)")
	  "Take the closure for" (pt* "f") ", extend its environment"
	  "with a binding for" (hbl-append (pt* "z") (t ",")) "and eval the closure's body")
  (eslide (point z-there z-there-frame) (program2.5 "10")
	  "Eval the RHS")
  (define (mk-xz-val xval vcolor)
    (add-binding* 0 #f "x" (colorize (alg-code xval) vcolor) #f z-there z-there-frame))
  (define-values (xz-there xz-there-frame) (mk-xz-val "10" green))
  (eslide (point xz-there xz-there-frame) (program2.5 "(let d.*|in x)")
	  "Add the binding for" (pt "x") "and eval the inner body")
  (eslide (point xz-there xz-there-frame) (program2.5 "set x.*")
	  "Eval RHS...")
  (define-values (xz-there-red xz-there-red-frame) (mk-xz-val "11" red))
  (eslide (point xz-there-red xz-there-red-frame) (program2.5 "set x.*")
	  "... which modifies the value of " (pt* "x"))
  (define-values (xz-there2 xz-there2-frame)
    (let-values ([(env frame) (mk-xz-val "11" green)])
      (add-binding* 0 #f "d" "1" #f env frame)))
  (eslide (point xz-there2 xz-there2-frame) (program2.5 "x$")
	  "Bind" (pt "d") "to" (pt "1") "and evaluate" (pt* "x") ","
	  " which produces" (pt* "11"))
  (eslide (point xz-there2 xz-there2-frame) (program2.5 ".f 2.")
	  "First operand is" (pt* "11") ";"
	  "now evaluate the second operand, " (pt* "(f 9)"))
  (define-values (zxz-there zxz-there-frame) (add-binding* 0 #t "z" "9" #f xz-there2 (env-frame empty-env)))
  (eslide (point zxz-there zxz-there-frame) (program2.5 "(let x.*|in let.*|in x)")
	  "Again, take the closure for" (pt* "f") ", extend the" (bt "closure's") "environment"
	  "with a binding for" (hbl-append (pt* "z") (t ",")) "and eval the closure's body")
  (define (mk-xzxz-val xval vcolor)
    (add-binding* 0 #f "x" (colorize (alg-code xval) vcolor) #f zxz-there zxz-there-frame))
  (define-values (xzxz-there xzxz-there-frame) (mk-xzxz-val "10" green))
  (eslide (point xzxz-there xzxz-there-frame) (program2.5 "(let d.*|in x)")
	  "Add a binding for" (pt "x") ", then eval the inner body")
  (define-values (xzxz-there-red xzxz-there-red-frame) (mk-xzxz-val "19" red))
  (eslide (point xzxz-there-red xzxz-there-red-frame) (program2.5 "set x.*")
	  "Again the" (pt* "d") "RHS modifies the value of" (pt* "x") 
	  ", but using the new" (pt "z") "and" (pt* "x"))
  (define-values (xzxz-there2 xzxz-there2-frame)
    (let-values ([(env frame) (mk-xzxz-val "19" green)])
      (add-binding* 0 #f "d" "1" #f env frame)))
  (eslide (point xzxz-there2 xzxz-there2-frame) (program2.5 "x$")
	  "Bind" (pt "d") "to" (pt "1") "and evaluate" (pt* "x") ","
	  "which produces" (pt* "19"))
  (eslide (point xzxz-there2 xzxz-there2-frame) (program2.5 "[+]..f.*")
	  "So the operands are" (pt* "11") "and" (hbl-append (pt* "19") (t ";"))
	  "The final result is" (pt* "30"))
  (eslide (no-point xzxz-there2) (program2.5 "!")
	  (key-point
	   half-page-para
	   "Every evaluation of a binding expression creates a new variable (box)"))

  (eslide-title "Assignment and Locals within Procedures")

  (define (program3 active)
    (alg-code$* active 
		"let mk = proc(x) proc(z)"
		"                  let d = set x = +(x,z) in x"
		" in let f = (mk 10)"
		"     in let g = (mk 12)   in ..."))

  (eslide (point empty-env) (program3 "!")
	  "An example with a procedure in a procedure")
  (eslide (point empty-env) (program3 "(proc.*|let d.*|in [+].*)")
	  "Eval RHS of the let expression...")
  (eslide (point (mk-mk/ #t blue) empty-env) (program3 "(proc.*|let d.*|in x)")
	  "... which creates a closure, pointing to the current environment")
  (eslide (point (mk-mk/ #f blue)) (program3 "(let f.*|in let g.*|in [.].*)")
	  "To finish the" (pt* "let") ", the environment is extended with"
	  (pt "mk") "bound to the closure, then evaluate the body")
  (eslide (point (mk-mk/ #f blue)) (program3 "[(]mk 10[)]")
	  "Eval RHS, a function call; look up" (pt* "mk") "...")
  (define-values (me mf) (mk-mk #f blue))
  (define-values (x-there x-there-frame) (add-binding* font-size #t "x" "10" #f me (env-frame empty-env)))
  (eslide (point x-there x-there-frame) (program3 "(proc.z.*|let d.*|in x)")
	  "It's a closure, so extend the closure's environment with"
	  (pt* "10") ", and eval the closure's body")
  (define-values (f-half-done _) (made-env #t "f" "10" mk-env mk-frame blue))
  (eslide (no-point f-half-done) (program3 "(proc.z.*|let d.*|in x)")
	  "Note that the variable" (pt "x") "is in the closure's"
	  "environment")
  (define-values (f-done f-done-frame) (made-env #f "f" "10" mk-env mk-frame blue))
  (eslide (point f-done) (program3 "(let g.*|in [.].*)")
	  "Bind" (pt "f") "to the closure, and evaluate the body")
  (eslide (point f-done) (program3 "[(]mk 12[)]")
	  "Eval RHS of the let expression,"
	  "another call to" (pt* "mk") "; same as before...")
  (define-values (g-done g-done-frame) (made-env #f "g" "12" f-done f-done-frame blue))
  (eslide (point g-done) (program3 "[.]..")
	  (quarter-page-para
	   "Extend" (hbl-append (pt* "mk") (t "'s")) "env with a new"
	   (pt "x") "and get a closure, this time bound to" (pt* "g")))

  (eslide (point g-done) (program3 "[.]..")
	  (quarter-page-para
	   "At this point," (pt "f") "and" (pt "g") "have private versions of"
	   (pt* "x")))

  (eslide (no-point g-done) (program3 "!")
	  (key-point
	   quarter-page-para
	   "Closures can capture generated variables, effectively getting private state"))

  (slide/title
   "Assignment Summary"
   (page-item  "Variables now denote references (a.k.a. locations), not values")
   (page-item  "Lexical scope still works")))
