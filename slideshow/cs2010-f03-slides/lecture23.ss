#cs
(module lecture23 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
           "utils/run.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
           (lib "list.ss"))

  (slide/title
   "Where are We?"
   (page-para "Part I: Basic software engineering")
   (page-subitem "How to represent things")
   (vc-append 
    line-sep
    (page-subitem "How to build programs around those representations")
    (page-para/r (colorize (it "Mid-term 1") RedColor)))
   (page-para "Part II: Scaling Up")
   (page-subitem "Abstraction")
   (vc-append 
    line-sep
    (page-subitem "Algorithms and state")
    (page-para/r (colorize (it "Mid-term 2") RedColor)))
   (colorize
    (vc-append
     gap-size
     (page-para "Part III: Another notation, more libraries")
     (page-subitem "Java"))
    "dark gray"))
  
  (define-syntax (lcode stx)
    (syntax-case stx ()
      [(_ . c) #'(hbl-append (blank (* 3 gap-size) 1) (code . c))]))
  
  (slide/title/tall
   "Advanced Scheme"
   (scale/improve-new-text
    (vl-append
     gap-size
     (vl-append
      line-sep
      (page-para* "A" (code <defn>) "is one of")
      (vl-append
       line-sep
       (lcode (define <var> <exp>))
       (lcode (define (<var> <var> ... <var>) <exp>))
       (lcode (define-struct <var> (<var> ... <var>)))))
     (vl-append
      line-sep
      (page-para* "An" (code <exp>) "is one of")
      (vl-append
       line-sep
       (lcode <var>)
       (lcode <con>)
       (lcode <prim>)
       (lcode (<exp> <exp> ... <exp>))
       (lcode (cond [<exp> <exp>] ... [<exp> <exp>]))
       (lcode (cond [<exp> <exp>] ... [else <exp>]))
       (lcode (and <exp> ... <exp>))
       (lcode (or <exp> ... <exp>))
       (lcode (local [<defn> ...] <exp>))
       (lcode (lambda (<var> ... <var>) <exp>))
       (lcode (set! <var> <exp>))
       (lcode (begin <exp> ... <exp>)))))
    0.9))
  
  (define (mini-scheme-exp-grammar scale)
    (scale/improve-new-text
     (vl-append
      line-sep
      (page-para* "An" (code <exp>) "is one of")
      (vl-append
       line-sep
       (lcode <var>)
       (lcode <num>)
       (lcode (+ <exp> <exp>))
       (lcode (- <exp> <exp>))
       (lcode (* <exp> <exp>))
       (lcode (<var> <exp>))))
     scale))
    
  (define (mini-scheme-grammar s)
    (scale/improve-new-text
     (vl-append
      gap-size
      (vl-append
       line-sep
       (page-para* "A" (code <defn>) "is one of")
       (lcode (define <var> <exp>))
       (lcode (define <var> (lambda (<var>) <exp>))))
      (vl-append
       line-sep
       (page-para* "An" (code <assign>) "is")
       (lcode (set! <var> <exp>)))
      (scale (mini-scheme-exp-grammar s) (/ s)))
     s))
  
  (slide/title
   "Mini Scheme"
   (mini-scheme-grammar 0.9))
  
  (slide/title/center
   "HW 10 and 11: Implementing DrMiniScheme"
   (gray-bitmap "mini-dr.png")
   'next
   (blank)
   (page-para "Key design problems for DrMiniScheme:")
   (page-subitem "Representing definitions and expressions")
   (page-subitem "Executing definitions and expressions")
   (page-subitem "Controlling the GUI"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define outline
    (make-outline
     'rep "Representing definitions and expressions" #f
     'gui "Converting strings to representations" #f
     'eval "Evaluating Expressions" #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'rep)
  
  (define three-fish
    (in-aq
     (vc-append
      (inset (hc-append (standard-fish 150 75 'right "red" "black")
                        (inset (standard-fish 100 50 'left "blue" "black")
                               gap-size 50 0 0))
             gap-size 0 0 0)
      (standard-fish 120 50 'left "green" "black"))))

  (define bubble-color (make-object color% 240 240 150))

  (define (imagine-bubble w h content)
    (vr-append
     (inset (cc-superimpose
             (cloud (* 6/5 w) (* 3/4 w) bubble-color)
             content)
            0 0 0 (- (* 1/8 w)))
     (colorize (filled-ellipse (* 1/6 w) (* 1/6 w)) bubble-color)
     (colorize (filled-ellipse (* 1/8 w) (* 1/8 w)) bubble-color)))
  
  (with-steps
   (drscheme aq aq-rep dr-empty mini mini-rep)
   (slide/title
    (if (before? dr-empty)
        "Implementing Aquariums in Advanced Scheme"
        "Implementing Mini Scheme in Advanced Scheme")
    (vc-append
     (- gap-size)
     ((vafter aq)
      (inset (imagine-bubble 
              300 600 
              (cc-superimpose
               ((vbefore dr-empty) (scale three-fish 0.5 0.5))
               ((vafter mini) (desktop-machine 1 '(plt)))))
             0 0 200 0))
     (desktop-machine 1.5 '(plt)))
    (lt-superimpose
     ((vonly aq-rep)
      (page-para "Represent fish, as opposed to stuffing"
                 (it "real") "fish into DrScheme"))
     ((vonly mini-rep)
      (page-para "Represent Mini Scheme expressions, as opposed to typing"
                 (it "real") "expressions into DrScheme")))))
     
   

  (slide/title
   "Representing Mini Scheme Expressions"
   (mini-scheme-exp-grammar 0.8)
   (colorize (hline (* 3/4 client-w) 1) GreenColor)
   'alts
   (list (list
          (page-para "We can't simply write")
          (code (+ 1 2))
          (page-para "to represent a Mini Scheme addition expression"))
         (list
          (page-para "We can write")
          (code '(+ 1 2))
          (page-para "which is almost as convenient!"))
         (list
          (page-para "To represent the" (code <var>) (code x) ":")
          (code 'x))
         (list
          (page-para "To represent the" (code <num>) (code 5) ":")
          (code '5)
          (page-para "which is actually just" (code 5)))
         (list
          (page-para "To represent the application" (code (f (+ 1 2))))
          (code '(f (+ 1 2)))
          (page-para "which is the same as")
          (code (list 'f (list '+ 1 2))))))
         
  
  (slide/title
   "Representing Mini Scheme Expressions"
   (page-para "Data definition:")
   (code (code:comment "A expr-snl is either")
         (code:comment "  - sym")
         (code:comment "  - num")
         (code:comment "  - (list '+ expr-snl expr-snl)")
         (code:comment "  - (list '- expr-snl expr-snl)")
         (code:comment "  - (list '* expr-snl expr-snl)")
         (code:comment "  - (list sym expr-snl)")))

  (slide/title
   "Representing Mini Scheme Expressions"
   (page-para "A better data definition in the long run:")
   (code (code:comment "A expr-snl is either")
         (code:comment "  - sym")
         (code:comment "  - num")
         (code:comment "  - add-expr-snl")
         (code:comment "  ...")
         (code:comment " ")
         (code:comment "An add-expr-snl is")
         (code:comment " (list '+ expr-snl expr-snl)")
         (code:comment " ")
         (code:comment "...")))
  
  (slide/title
   "Representing Definitions and Assignments"
   (code (code:comment "A defn-snl is either")
         (code:comment "  - (list 'define sym expr-snl)")
         (code:comment "  - (list 'define sym ")
	 (code:comment "          (list 'lambda (list sym)")
	 (code:comment "                expr-snl))")
         (code:comment " ")
         (code:comment "An assign-snl is")
         (code:comment " (list 'set! sym expr-snl)")))

  (slide/title
   "HW 10"
   (page-para "HW 10 simplification: only define/assign to numbers,"
              "and only evaluate variable names")
   (blank)
   (code (code:comment "A defn-snl is")
         (code:comment " (list 'define sym num)")
         (code:comment " ")
         (code:comment "An assign-snl is")
         (code:comment " (list 'set! sym num)")
         (code:comment " ")
         (code:comment "An expr-snl is")
         (code:comment " sym")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'gui)
  
  (slide/title
   "Converting a String to a Mini Scheme Expression"
   (page-para "In the GUI, the defininition/assignment/expression is available only as a string:")
   (gray-bitmap "mini-dr.png")
   'next
   (blank)
   (page-para "The" (code read-from-string) "teachpack function converts"
              "a string by putting a quote in front of it")
   (code (read-from-string "1") #,sym:rightarrow 1
         (read-from-string "(+ 1 2)") #,sym:rightarrow '(+ 1 2)
         (read-from-string "(define x 7)")
         code:blank #,sym:rightarrow '(define x 7)))

  (slide/title
   "The snl Datatype"
   (code (code:contract read-from-string : string -> snl)
         code:blank
         (code:comment "An snl is either")
         (code:comment "  - sym")
         (code:comment "  - num")
         (code:comment "  - list-of-snl"))
   'next
   (blank)
   (page-para "Example" (hbl-append (code snl) (t "s")) ":")
   (code 'x
         1
         '(1 1 1 x 1))
   'next
   (blank)
   (colorize (page-para* "Not every" (code snl) "is a" (code defn-snl)
                         "," (code assign-snl) ", or" (code expr-snl))
             BlueColor))

  (slide/title
   "Checking for Definitions"
   (code (code:contract is-defn? : snl -> bool)
         (define (is-defn? s)
           (and (list? s)
                (= 3 (length s))
                (symbol? (first s))
                (symbol=? 'define (first s))
                (symbol? (second s))
		(number? (third s))))))

  (slide/title
   "Checking for Expressions (HW 11)"
   (scale/improve-new-text
    (code (code:contract is-expr? : snl -> bool)
	  (define (is-expr? s)
	    (or (number? s)
		(symbol? s)
		(is-plus? s)
		...))
	  code:blank
	  (code:contract is-plus? : snl -> bool)
	  (define (is-plus? s)
	    (and (list? s)
		 (= 3 (length s))
		 (symbol? (first s))
		 (symbol=? '+ (first s))
		 (is-expr? (second s))
		 (is-expr? (third s))))
	  code:blank
	  ...)
    0.8))

  (slide/title
   "Executing Code"
   (gray-bitmap "mini-dr.png")
   (blank)
   (page-para "When the" (bt "Eval") "button is clicked:")
   (page-item "If it's a definition, record it")
   (page-item "If it's an assignment, do it")
   (page-item "If it's an expression, evaluate it"))

  (slide/title
   "Execution"
   (code (code:contract execute : snl -> string)
         (define (execute s)
           (cond
             [(is-defn? s) ...]
             [(is-assign? s) ...]
             [(is-expr? s) ...]
             [else "bad input"]))
         ...
         (code:contract execute-string : snl -> string)
         (code:comment " Used by the Execute button callback")
         (define (execute-string str)
           (local [(define snl (read-from-string str))]
             (cond
               [(boolean? snl) "bad input"]
               [else (execute s)])))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'eval)

  (slide/title
   "Evaluating Mini Scheme (HW 11)"
   (page-para
    (code (evaluate '3) "should be" 3
	  (evaluate '(+ 1 2)) "should be" 3
	  (evaluate '(+ 1 (* 2 5))) "should be" 11))
   'next
   (blank)
   (page-para "Assuming" (code (define f (lambda (x) (+ x 1)))) ":")
   'alts
   (list (list
	  (page-para
	   (code (evaluate '(f 7)))))
	 (list
	  (page-para
	   (code (evaluate '(f 7))
		 "should be" 8))
	  (page-para/r "involves substituting" (code 7) "into" (code (+ x 1))))))

  (slide/title
   "Evaluating Mini Scheme"
   (scale/improve-new-text
    (code (code:contract evaluate : expr-snl -> value)
	  (define (evaluate s)
	    (cond
             [(number? s) ...]
             [(symbol? s) ...]
             [(is-plus? s) ... (evaluate-plus s) ...]
             [(is-minus? s) ... (evaluate-minus s) ...]
             [(is-times? s) ... (evaluate-times s) ...]
             [(is-app? s) ... (evaluate-app s) ...])))
    0.9))
  
  (slide/title
   "Evaluating Mini Scheme"
   (code (code:comment "...")
         (code:comment "A plus-expr-snl is")
         (code:comment " (list '+ expr-snl expr-snl)")
         (code:comment "...")
         code:blank
         (code:contract evaluate-plus : plus-expr-snl -> value)
         (define (evaluate-plus s)
           ... (evaluate (second s))
           ... (evaluate (third s))
           ...)))

  (slide/title
   "Evaluating Mini Scheme"
   (code (code:contract evaluate-app : plus-expr-snl -> value)
         (define (evaluate-app s)
           ... (first s) 
           ... (evaluate (second s))
           ...))
   'next
   (blank)
   (page-para "Assuming the first is defined as a function, the next step is"
              "to substitute..."))
  
  (slide/title
   "Substitution"
   (page-para "Assuming" (code (define f (lambda (x) (+ x 1)))) ":")
   (page-para
    (code (evaluate-app '(f (- 20 5)))
	  code:blank #,sym:rightarrow #,sym:rightarrow (substitute 'x 15 '(+ 1 x))))
   'next
   (blank)
   (page-para
    (code (substitute 'x 15 '(+ 1 x))
	  "should be" '(+ 1 15))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (slide/title
   "Summary"
   (page-para "HW 10")
   (page-item "Just definitions, assignments, variable lookup")
   (blank)
   (page-para "HW 11")
   (page-item "Expression evaluation")
   (page-item "Optional exercises: errors, conditionals"))

  
  )
