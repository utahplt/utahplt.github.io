#cs
(module lecture21 (lib "slideshow.ss" "slideshow")  
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

  (define-code 
   (simple-run simple-prog)
   (define TOTAL 0)
   code:blank
   (define total-message
     (make-message (number->string TOTAL)))
   (define amount-text
     (make-text "Amount"))
   code:blank
   (define add-button
     (make-button "+" 
                  (lambda (evt)
                    (add-to-total
                     (string->number (text-contents amount-text))))))
   code:blank
   (code:comment "add-to-total : num -> true")
   (define (add-to-total amt)
     (local [(define new-total (+ TOTAL amt))]
       (draw-message total-message (number->string new-total))))
   code:blank
   (create-window (list (list total-message)
                        (list amount-text)
                        (list add-button))))

  (define-code 
   (simple-accum-run simple-accum-prog)
   _
   (define TOTAL 0)
   _
   code:blank
   _
   (define total-message
     (make-message (number->string TOTAL)))
   _
   (define amount-text
     (make-text "Amount"))
   _
   code:blank
   _
   (define add-button
     (make-button "+" 
                  (lambda (evt)
                    (add-to-total
                     (string->number (text-contents amount-text))))))
   _
   code:blank
   _
   (code:comment "add-to-total : num -> true")
   (define (add-to-total amt)
     (local [(define new-total (+ TOTAL amt))]
       (begin
         (set! TOTAL new-total)
         (draw-message total-message (number->string new-total)))))
   _
   code:blank
   _
   (create-window (list (list total-message)
                        (list amount-text)
                        (list add-button))))



  (define-code
   (infix-run infix-code)
   _
   (define TOTAL 0)
   _
   (define WORKING 0)
   ...
   (define PREV-OP +)
   _
   code:blank
   _
   (define total-message
     (make-message (number->string TOTAL)))
   code:blank
   (code:comment "op-button : string (num num -> num) -> button")
   (define (op-button label OP)
     (make-button label
                  (lambda (evt)
                    (begin
                      (change-total PREV-OP WORKING)
                      (set! PREV-OP OP)
                      true))))
   _
   code:blank
   _
   (code:comment "digit-button : num -> button")
   _
   (define (digit-button n)
     (make-button (number->string n)
                  (lambda (evt)
                    (add-digit n))))
   _
   code:blank
   _
   (code:comment "add-digit : num -> true")
   _
   (define (add-digit n)
     (begin
       (set! WORKING (+ n (* WORKING 10)))
       (draw-message total-message (number->string WORKING))))
   _
   code:blank
   _
   (code:comment "change-total : (num num -> num) num -> true")
   _
   (define (change-total OP amt)
     (local [(define new-total (OP TOTAL amt))]
       (begin
         (set! TOTAL new-total)
         (set! WORKING 0)
         (draw-message total-message (number->string new-total)))))
   ...
   code:blank
   (create-window (list (list total-message)
                        (map digit-button '(7 8 9))
                        (map digit-button '(4 5 6))
                        (map digit-button '(1 2 3))
                        (map digit-button   '(0))
                        (list (op-button "+" +)
                              (op-button "-" -)
                              (op-button "*" *)
                              (op-button "/" /)
                              (op-button "=" (lambda (tot new) new))))))

  (define-code
   (multi-run multi-code)
   (define (make-calculator)
     (local [(define TOTAL 0)
             (define WORKING 0)
             ...
             _
             (define PREV-OP +)
             _
             (define total-message
               (make-message (number->string TOTAL)))
             _
             (code:comment "op-button : string (num num -> num) -> button")
             _
             (define (op-button label OP)
               (make-button label
                            (lambda (evt)
                              (begin
                                (change-total PREV-OP WORKING)
                                (set! PREV-OP OP)
                                true))))
             _
             (code:comment "digit-button : num -> button")
             _
             (define (digit-button n)
               (make-button (number->string n)
                            (lambda (evt)
                              (add-digit n))))
             _
             (code:comment "add-digit : num -> true")
             _
             (define (add-digit n)
               (begin
                 (set! WORKING (+ n (* WORKING 10)))
                 (draw-message total-message (number->string WORKING))))
             _
             (code:comment "change-total : (num num -> num) num -> true")
             _
             (define (change-total OP amt)
               (local [(define new-total (OP TOTAL amt))]
                 (begin
                   (set! TOTAL new-total)
                   (set! WORKING 0)
                   (draw-message total-message (number->string new-total)))))]
       (create-window
        (list (list total-message)
              (map digit-button '(7 8 9))
              (map digit-button '(4 5 6))
              (map digit-button '(1 2 3))
              (map digit-button   '(0))
              (list (op-button "+" +)
                    (op-button "-" -)
                    (op-button "*" *)
                    (op-button "/" /)
                    (op-button "=" (lambda (tot new) new)))))))
   (make-calculator)
   (make-calculator))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define infix-bitmap
    (gray-bitmap "infix-calc.png"))
  
  (slide/title/center
   "Calculator"
   infix-bitmap
   (mk-run infix-run))

  (slide/title/center
   "Adding Machine"
   (gray-bitmap "adder-calc.png")
   (mk-run simple-run))
 
  (slide/title
   "Simple Adder"
   simple-prog)
  
  (slide/title
   "Why the Adder is Unlike A Calculator"
   (scode
    (define (add-to-total amt)
      (local [(define new-total (+ TOTAL amt))]
	(draw-message total-message (number->string new-total)))))
   (blank)
   (page-item "Every time we have a new" (code amt)
              ", it's added to the same original" (code TOTAL))
   (page-item "The new total is drawn on the screen, then forgotten")
   'next
   (blank)
   (colorize
    (page-para* "We need to remember" (code new-total) "for next time")
    BlueColor))
  
  (slide/title
   "set!"
   (page-para "In" (bt "Advanced") ":")
   (code (set! TOTAL 17))
   (page-para "changes the value of" (code TOTAL) "to 17")
   'next
   (blank)
   (blank)
   (page-item "\"Constant\" definitions are no longer constant"
              sym:emdash "the are" (dt "variable definitions"))
   (page-item "A" (code set!) "expression is called an" (dt "assignment"))
   (page-item "The value of" (code TOTAL) "is the" (dt "state") "of the  program"))
  
  (define-syntax (ecode stx)
    (syntax-case stx ()
      [(_  . rest)
       #'(scale/improve-new-text (code . rest) 1.0)]))
  
  (eval-steps
   "Evaluating set!"
   (ecode (define TOTAL 0)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (add-amt 1)
          (add-amt 2))
   #f
   (ecode (define TOTAL 0)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (set! TOTAL (+ TOTAL 1))
          (add-amt 2))
   #f
   (ecode (define TOTAL 0)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (set! TOTAL (+ 0 1))
          (add-amt 2))
   #f
   (ecode (define TOTAL 0)
          (define (add-amt amt) 
            (set! TOTAL (+ TOTAL amt)))
          (set! TOTAL 1)
          (add-amt 2))
   (page-para "To evaluate" (code set!) ", change a definition and produce" (code (void)))
   (ecode (define TOTAL 1)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (void)
          (add-amt 2))
   #f
   (ecode (define TOTAL 1)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (void)
          (set! TOTAL (+ TOTAL 2)))
   #f
   (ecode (define TOTAL 1)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (void)
          (set! TOTAL (+ 1 2)))
   (page-para "It's important that a variable name is not replaced by its"
              "value until the value is needed")
   (ecode (define TOTAL 1)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (void)
          (set! TOTAL 3))
   #f
   (ecode (define TOTAL 3)
          (define (add-amt amt)
            (set! TOTAL (+ TOTAL amt)))
          (void)
          (void)))
  
  (slide/title
   "Repairing the Calculator"
   (scode
    (define (add-to-total amt)
      (local [(define new-total (+ TOTAL amt))]
        (code:comment "How do we combine two actions?")
        ...
        (set! TOTAL new-total)
        (draw-message total-message (number->string new-total))
        ...)))
   'next
   (blank)
   (page-item "For drawing, we used" (code and) "to combine actions")
   (page-item "But" (code set!) "doesn't return a boolean"))
  
  
  (slide/title
   "Making the Adder Remember Totals"
   (page-para "Also new in" (bt "Advanced") ": the" (code begin) "form")
   simple-accum-prog
   'next
   (blank)
   (page-para "The" (code begin) "form")
   (page-item "Evaluates its first expression")
   (page-item "Throws away the result")
   (page-item "Goes away when only one expression is left")
   (page-para (code begin) "works with any number of expressions")
   (mk-run simple-accum-run))
  
  (eval-steps
   "Evaluating begin"
   (ecode (define TOTAL 3)
          (define (running-total amt)
            (begin
              (set! TOTAL (+ TOTAL amt))
              TOTAL))
          (running-total 10))
   #f
   (ecode (define TOTAL 3)
          ...
          (begin
            (set! TOTAL (+ TOTAL 10))
            TOTAL))
   #f
   (ecode (define TOTAL 3)
          ...
          (begin
            (set! TOTAL (+ 3 10))
            TOTAL))
   #f
   (ecode (define TOTAL 3)
          ...
          (begin
            (set! TOTAL 13)
            TOTAL))
   #f
   (ecode (define TOTAL 13)
          ...
          (begin
            (void)
            TOTAL))
   #f
   (ecode (define TOTAL 13)
          ...
          (begin
            TOTAL))
   #f
   (ecode (define TOTAL 13)
          ...
          TOTAL)
   #f
   (ecode (define TOTAL 13)
          ...
          13))

  (define-code
   (arith-run arith-code)
   _
   (define TOTAL 0)
   _
   code:blank
   _
   (define total-message
     (make-message (number->string TOTAL)))
   _
   (define amount-text
     (make-text "Amount"))
   ...
   code:blank
   (code:comment "op-button : string (num num -> num) -> button")
   (define (op-button label OP)
     (make-button label
                  (lambda (evt)
                    (change-total
                     OP
                     (string->number (text-contents amount-text))))))
   code:blank
   (code:comment "change-total : (num num -> num) num -> true")
   (define (change-total OP amt)
     (local [(define new-total (OP TOTAL amt))]
       (begin
         (set! TOTAL new-total)
         (draw-message total-message (number->string new-total)))))
   code:blank
   (create-window (list (list total-message)
                        (list amount-text)
                        (list (op-button "+" +)
                              (op-button "-" -)
                              (op-button "*" *)
                              (op-button "/" /)))))

  (slide/title/tall
   "More Calculator Buttons"
   (gray-bitmap "arith-calc.png")
   (mk-run arith-run))

  (slide/title/tall
   "Implementing More Calculator Buttons"
   arith-code)
  
  (define-code 
   (digit-run digit-code)
   _
   (define TOTAL 0)
   ...
   (define WORKING 0)
   _
   code:blank
   _
   (define total-message
     (make-message (number->string TOTAL)))
   _
   code:blank
   _
   (code:comment "op-button : string (num num -> num) -> button")
   _
   (define (op-button label OP)
     (make-button label
                  (lambda (evt)
                    (change-total OP WORKING))))
   code:blank
   (code:comment "digit-button : num -> button")
   (define (digit-button n)
     (make-button (number->string n)
                  (lambda (evt)
                    (add-digit n))))
   code:blank
   (code:comment "add-digit : num -> true")
   (define (add-digit n)
     (begin
       (set! WORKING (+ n (* WORKING 10)))
       (draw-message total-message (number->string WORKING))))
   code:blank
   (code:comment "change-total : (num num -> num) num -> true")
   (define (change-total OP amt)
     (local [(define new-total (OP TOTAL amt))]
       (begin
         (set! TOTAL new-total)
         (set! WORKING 0)
         (draw-message total-message (number->string new-total)))))
   ...
   _
   code:blank
   _
   (create-window (list (list total-message)
                        (map digit-button '(7 8 9))
                        (map digit-button '(4 5 6))
                        (map digit-button '(1 2 3))
                        (map digit-button   '(0))
                        (list (op-button "+" +)
                              (op-button "-" -)
                              (op-button "*" *)
                              (op-button "/" /)))))
  
  (slide/title
   "The Digit Buttons"
   (gray-bitmap "digit-calc.png")
   (mk-run digit-run)
   (page-para "Now two pieces of state:")
   (page-item "The running total")
   (page-item "The number we're typing, so far"))

  (slide/title/tall
   "Implementing Digit Buttons"
   digit-code)

  (slide/title
   "Infix Operations"
   infix-bitmap
   (mk-run infix-run)
   (page-para "A normal calculator uses infix (algebra-like) order")
   (page-para "New piece of state:")
   (page-item "The operation to perform when the number is ready"))

  (slide/title/tall
   "Implementing Infix Operations"
   infix-code)
  
  (slide/title
   "Multiple Calculators"
   (ht-append (* gap-size 3)
              infix-bitmap
              (inset
               (gray-bitmap "infix2-calc.png")
               0 gap-size 0 0))
   (mk-run multi-run)
   (blank)
   (page-para "How can we keep the calculators from using the same" (code TOTAL) "?")
   'next
   (blank)
   (colorize
    (page-para* "Easy" sym:emdash "use" (code local))
    BlueColor))

  (slide/title
   "Implementing Multiple Calculators"
   multi-code)

  (slide/title
   "When to use State"
   (page-para "Use state and" (code set!) "when")
   (page-item "a function needs to remember something about previous calls, and")
   (page-item "you have no control over the callers"))
  
  (slide/title
   "When NOT to use State"
   (page-para "The following is a unacceptable use of" (code set!))
   (code (define REV empty)
         (define (reverse-list l)
           (cond
             [(empty? l) REV]
             [(cons? l) 
              (begin
                (set! REV (cons (first l) REV))
                (reverse-list (rest l)))]))
         (reverse-list '(1 2 3 4 5)))
   (blank)
   (page-item "Recursive calls build on earlier results, but"
              "we control all of the recursive calls")
   (page-item "It produces the wrong result when you call it a second time"))
   
   
  
  )

