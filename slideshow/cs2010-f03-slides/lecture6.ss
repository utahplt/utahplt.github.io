#cs
(module lecture6 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (define outline
    (make-outline
     'dd+tmpl "Data Definitions and Templates" #f
     'syntax+semantics "Syntax and Semantics" #f
     'checks "Defensive Programming" #f))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'dd+tmpl)
     
  (define w-defn
    (code (code:comment "A w-grade is either")
	  (code:comment " - num")
	  (code:comment " - posn")
	  (code:comment " - empty")))
  (define z-defn
    (code (code:comment "A z-grade is either")
	  (code:comment " - num")
	  (code:comment " - (make-posn num num)")
	  (code:comment " - empty")))

  (define posn-defn
    (code (code:comment "A posn is")
	  (code:comment " (make-posn num num)")))

  (define both-defns
    (vc-append
     gap-size
     (vc-append
      line-sep
      (lc-superimpose w-defn (ghost z-defn))
      (page-para/r
       (htl-append (t "with ") posn-defn)))
     (blank)
     (lc-superimpose z-defn (ghost w-defn))))

  (define (question n . l)
    (vl-append
     line-sep
     (page-para (colorize (bt (format "Question ~a:" n)) GreenColor))
     (apply page-para l)))

  (slide/title/tall
   "Data Definitions"
   (question 1 "Are both of the following data definitions ok?")
   both-defns
   'next
   (colorize (page-para (bt "Yes.")) BlueColor))

  (slide/title/tall
   "Data Definitions"
   (question 2 "Do" (code w-grade) "and" (code z-grade) "identify the same set of values?")
   both-defns
   'next
   (vl-append
    line-sep
    (page-para (colorize (bt "Yes,") BlueColor)
	       "every" (code w-grade) "is a" (code w-grade) ",")
    (page-para "and every" (code z-grade) "is a" (code w-grade))))

  (slide/title/tall
   "Data Definitions"
   (question 3 "Are" (code w-grade) "and" (code w-grade) "the same data definition?")
   both-defns
   'next
   (page-para (colorize (bt "No,") BlueColor)
	       "in the sense that they generate different templates"))

  (slide/title
   "Data Definitions and Templates"
   (page-para "The template depends on the" (it "static")
	      "," (it "textual") "content of a data definition, only")
   (blank)
   (hc-append
    (scale 
     (table 2
	    (list
	     (add-gp-arrow
	      (vl-append
	       line-sep
	       w-defn
	       posn-defn)
	      1/3 25/60 1/3 45/60)
	     (add-gp-arrow
	      (code
	       (define (func-for-w-grade w)
		 (cond
		  [(number? w) ...]
		  [(posn? w) ... (func-for-posn w) ...]
		  [(empty? w) ...]))
	       (define (func-for-posn p)
		 ... (posn-x p) ... (posn-y p) ...))
	      1/2 35/70 1/3 55/70)
	     
	     z-defn
	     (code
	      (define (func-for-z-grade z)
		(cond
		 [(number? z) ...]
		 [(posn? z) ... (posn-x z) ... (posn-y z) ...]
		 [(empty? z) ...]))))
	    lt-superimpose lt-superimpose 
	    (* 3 gap-size) (* 4 gap-size))
     0.6 0.6)))

  (slide/title
   "Data Definitions and Templates"
   (page-para "Why we treat the data definition statically"
	      "to generate a template:")

   (page-item "Provides well-defined, simple rules for generating a template")

   (page-subitem "\"Dynamic\" coverage is difficult in general")
   (page-subitem "Recall 3520 anecdote: thinking in terms of dynamic coverage"
		 sym:implies "broken programs")

   (page-item "Similar to the way that data choices affect modularity")

   (page-subitem "Details of modularity are beyond the scope of this class,"
		 "but we want to build the right instincts"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'syntax+semantics)

  (define ex-title
    "Execution in DrScheme")
  (define pre-label
    (page-para "Suppose that DrScheme's definition window contains"))
  (define post-label
    (inset
     (page-para "What's the result of clicking" (bt "Execute") "?")
     0 (* 2 gap-size) 0 0))
  (define (answer . l)
    (apply page-para/c l))
  (define (errmsg s)
    (colorize (text s `(italic . modern) font-size) RedColor))
  
  (define no-result
    (answer "Nothing (although" (code f) "would produce an error if it were used)"))
  
  (slide/title
   ex-title
   pre-label
   (code (define (f x)
           (/ x 2))
         (f 10))
   post-label
   'next
   (answer (code 5)))
  
  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (/ x 0))
         (f 10))
   post-label
   'next
   (answer (errmsg "/: divide by 0")))
  
  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (/ x 0)))
   post-label
   'next
   no-result)  

  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (/ x (0))))
   post-label
   'next
   (answer (errmsg "expected a name after an open parenthesis,")
           (errmsg "found a number")
           (symbol 190)
           "even without using" (code f)))

  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (cond x)))
   post-label
   'next
   (answer (errmsg "cond: expected a question--answer clause")
           (symbol 190)
           "even without using" (code f)))

  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (cond
             [false x])))
   post-label
   'next
   (answer "Nothing"))
  
  (slide/title
   "Execution in DrScheme"
   pre-label
   (code (define (f x)
           (cond
             [false x]))
         (f 10))
   post-label
   'next
   (answer (errmsg "cond: all questions were false")))

  (slide/title
   "Errors in DrScheme"
   (page-para "DrScheme complains about a function body")
   (page-subitem "sometimes before the function is used")
   (page-subitem "sometimes only when the function is called")
   (colorize (page-para "Why?") BlueColor)
   'next
   (blank)
   (page-para* "Because some errors are" (dt "syntax errors")
               "and some errors are" (dt "run-time errors")))

  (define (err-ex a b) (vl-append line-sep a b))
  
  (slide/title
   "Syntax Errors"
   (page-para "A" (dt "syntax error") "is like a question that isn't a well-formed sentence")
   (blank)
   (err-ex
    (page-item (code f(x) = x + 0))
    (page-subitem "DrScheme doesn't understand this notation, just like..."))
   (err-ex
    (page-item "\"Parlez-Vous Fran\347ais ?\"")
    (page-subitem "English-only speaker doesn't understand this notation"))
   'next
   (blank)
   (err-ex
    (page-item (code (define (f x) (/ x (0)))))
    (page-subitem "Parens around a zero make no sense to DrScheme, just like..."))
   (err-ex
    (page-item "\"Does rain dog cat?\"")
    (page-subitem "Not enough verbs for this to make sense in English"))
   'next
   (blank)
   (page-para "When DrScheme sees a syntax error, it refuses to evaluate"))
  
  (slide/title
   "Run-Time Errors"
   (page-para "A" (dt "run-time error") "is like a well-formed question with no answer")
   (blank)
   (err-ex
    (page-item (code (/ 12 0)))
    (page-subitem "A clear request to DrScheme, but no answer, just like..."))
   (err-ex
    (page-item "\"Why are you wearing a green hat?\"")
    (page-subitem "There's no answer if I'm wearing a blue hat"))
   'next
   (blank)
   (err-ex
    (page-item (code (cond [false 10])))
    (page-subitem "There's no reasonable choice for DrScheme, just like..."))
   (err-ex
    (page-item "\"If you can't understand me, what's your name?\"")
    (page-subitem "No one who understands the question should answer"))
   'next
   (blank)
   (page-para "DrScheme evaluates around run-time errors until forced to answer"))
  
  (slide/title/center
   "The Difference between DrScheme and English"
   (page-para "In a (good) programming language, all errors are well-defined, and"
	      "the rules are relatively simple")
   (blank)
   (page-item "DrScheme has a simple, well-defined grammar, and deviations from"
              "the grammar are syntax errors")
   (page-item "The reduction rules for each construct and primitive operation"
              "are well-defined, producing either a value or an error"))
  
  (slide/title/tall
   "Beginner Scheme Grammar"
   (page-para "A" (code <var>) "is a name, a"
              (code <con>) "is a constant, and a"
              (code <prm>) "is an operator name")
   (vc-append
    line-sep
    (page-para "A" (code <defn>) "is one of")
    (vl-append
     line-sep
     (code (define (<var> <var> ... <var>) <exp>))
     (code (define <var> <exp>))
     (code (define-struct <var> (<var> ... <var>)))))
   (vc-append
    line-sep
    (page-para "A" (code <exp>) "is one of")
    (vl-append
     line-sep
     (code <var>)
     (code <con>)
     (code (<prm> <exp> ... <exp>))
     (code (<var> <exp> ... <exp>))
     (code (cond [<exp> <exp>] ... [<exp> <exp>]))
     (code (cond [<exp> <exp>] ... [else <exp>]))
     (code (and <exp> ... <exp>))
     (code (or <exp> ... <exp>)))))

  
  (slide/title
   "Evaluation Rules: and/or"
   (vr-append
    line-sep
    (page-para (code (and true ... true false _question ... _question)))
    (page-para* rightarrow (code false))
    (page-para (code (and true ... true)) rightarrow (code true)))
   (blank)
   (vr-append
    line-sep
    (page-para (code (or false ... false true _question ... _question)))
    (page-para* rightarrow (code true))
    (page-para (code (or false ... false)) rightarrow (code false)))
   'next
   (blank)
   (page-para "Note that")
   (code (and 7 false))
   (page-para "fits the grammar, but has no matching evaluation rule, so it produces a run-time error"))
   
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'checks)

  (slide/title
   ex-title
   pre-label
   (code (code:contract f : num -> num)
         (define (f x)
           (+ x 2))
         (f 'apple))
   post-label
   'next
   (answer (errmsg "+: expects a <number>, given 'apple"))
   'next
   (blank)
   (colorize (page-para* "But this is really a contract violation at the call to" (code f)) BlueColor)
   (page-para "The implementor of" (code f) "might want to clarify that this"
              "error is someone else's fault, not a bug in" (code f)))

  (slide/title
   "Defensive Programming"
   (code (code:contract f : num -> num)
         (define (real-f x)
           (+ x 2))
         (define (f x)
           (cond
             [(number? x) (real-f x)]
             [else (error 'f "not a number")]))
         (f 'apple))
   'next
   (answer (errmsg "f: not a number"))
   'next
   (blank)
   (page-para "The" (code error) "function triggers a run-time error")
   'next
   (page-para "You don't have to program defensively in this course, but"
              "it sometimes helps to defend against your own mistakes!"))
  )
