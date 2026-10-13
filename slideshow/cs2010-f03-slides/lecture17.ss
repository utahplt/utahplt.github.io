#cs
(module lecture17 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "list.ss"))
  
  (define down-arrow
    (colorize (arrow gap-size (* -1/2 pi)) GreenColor))
  
  (define (howto-step txt)
    (frame (inset (t txt) (/ gap-size 2))))
  
  (define (hts-append sep . l)
    (let ([g (ghost (apply cc-superimpose l))])
      (apply ht-append sep (map (lambda (i) (cc-superimpose g i)) l))))
  
  (define rep-step (howto-step "Data Representation and Contract"))
  (define example-step (howto-step "Examples"))
  
  (define template-branch
    (vc-append
     gap-size
     (howto-step "Template")
     down-arrow
     (howto-step "Body")))
  (define reuse-branch
    (vc-append
     gap-size
     (howto-step "Maybe Abstract")
     down-arrow
     (howto-step "Use Existing")))
  
  (slide/title
   "How to Design A Program (So Far)"
   (vc-append
    gap-size
    rep-step
    down-arrow
    example-step
    (hc-append (* 3 gap-size)
               (colorize (arrow gap-size (* -3/4 pi)) GreenColor)
               (colorize (arrow gap-size (* -1/4 pi)) GreenColor))
    (hts-append
     (* 2 gap-size)
     reuse-branch
     template-branch)
    (hc-append (* 3 gap-size)
               (colorize (arrow gap-size (* -1/4 pi)) GreenColor)
               (colorize (arrow gap-size (* -3/4 pi)) GreenColor))
    (howto-step "Test")))
    
  (slide/title/center
   "Challenge Problem"
   (problem "Implement the function" (code odd-items) "which takes a list-of-X"
            "and produces a list-of-X containing every other item in the"
            "given list (including the first item)"))
  
  (slide
   rep-step
   (blank)
   (page-para "Already done for us:")
   (code (code:contract odd-items :  list-of-X -> list-of-X)))
  
  (slide
   example-step
   (blank)
   (code (odd-items empty) "should be" empty
         code:blank
         (odd-items '(1 2 3 4 5)) 
         "should be" '(1 3 5)
         code:blank
         (odd-items '(apple banana cherry)) 
         "should be" '(apple cherry)
         code:blank
         (odd-items (list true false))
         "should be" (list true)))
  
  (slide
   (hc-append (* 2 gap-size)
              reuse-branch
              (bt "or")
              template-branch
              (bt "?"))
   'next
   (blank)
   (blank)
   (page-para "We know that" (code foldr) "captures"
              "the template for" (code list-of-X) ", so"
              "choose the left branch" sym:emdash
              "and abstraction is done already!"))
  
  (slide
   reuse-branch
   (blank)
   (code (define (odd-items l)
           (foldr (lambda (item odd-rest)
                    ...)
                  empty l)))
   'next
   (colorize (page-para "Problem: the odd items of the rest of the list are"
                        "useless for the odd items of the whole list")
             RedColor)
   (code (odd-items '(1 2 3 4)) "should be" '(1 3))
   (colorize (t "but") BlueColor)
   (code (odd-items '(2 3 4)) "should be" '(2 4)))
  
  (slide
   (hc-append (* 3 gap-size) template-branch (bt "?"))
   'next
   (blank)
   (code (define (odd-items l)
           (cond
             [(empty? l) empty]
             [(cons? l) 
              ... (first l)
              ... (odd-items (rest l)) ...])))
   (colorize (page-para "Same problem" sym:emdash
                        "it's not just a reuse problem...")
             RedColor))
  
  (slide/title
   "Structural Recursion"
   (page-item "For recursively defined data, our recipe so far always"
              "produces" (dt "structurally recursive") "programs")
   'next
   (blank)
   (page-item "In a sense, it always works:")
   (code (define (odd-items l)
           (first
            (foldr (lambda (item odds+evens)
                     (list (cons item 
                                 (second odds+evens))
                           (first odds+evens)))
                   (list empty empty) l))))
   (blank)
   (colorize
    (page-para*/c "But making structural recursion work"
                  "sometimes requires more creativity than solving the problem"
                  "a different way")
    BlueColor))
  
  (slide/title
   "Generative Recursion"
   (page-para "Structural recursion is a powerful tool, but we need more tools")
   'next
   (page-para "Our new tool is" (dt "generative recursion") ":")
   (code (define (func v)
           (cond
             [(_trivially-solvable? v) ...]
             [else ...
                   (func _generated-v_1)
                   code:blank #,(code-align (text "..." `(bold . modern) (current-font-size) (/ pi 2)))
                   (func _generated-v_n)
                   ...])))
   (blank)
   (colorize
    (page-para*/c "Structural recursion is a special case of generative recursion"
                  "that is especially common")
    BlueColor))
  
  (slide/title
   "Back to Odd Items"
   'alts
   (list (list
          (page-para "When the list given to" (code odd-items)
                     "has less than two items, the problem is trivial to solve:")
          (code (define (odd-items l)
                  (cond
                    [(or (empty? l) 
                         (empty? (rest l))) 
                     l]
                    [else ...]))))
         (list
          (page-para "Otherwise, it's helpful to have the"
                     (code rest) "of the" (code _rest) ":")
          (code (define (odd-items l)
                  (cond
                    [(or (empty? l) 
                         (empty? (rest l))) 
                     l]
                    [else (cons 
                           (first l)
                           (odd-items (rest (rest l))))]))))))

  (define genrec-branch
    (vc-append
     gap-size
     (howto-step "Trivial Cases")
     down-arrow
     (howto-step "Recur on Smaller")))
      
  (slide/title
   "How to Design A Program"
   (vc-append
    gap-size
    rep-step
    down-arrow
    example-step
    (hc-append (* 6 gap-size)
               (colorize (arrow gap-size (* -3/4 pi)) GreenColor)
               down-arrow
               (colorize (arrow gap-size (* -1/4 pi)) GreenColor))
    (hts-append
     (* 2 gap-size)
     reuse-branch
     template-branch
     genrec-branch)
    (hc-append (* 6 gap-size)
               (colorize (arrow gap-size (* -1/4 pi)) GreenColor)
               down-arrow
               (colorize (arrow gap-size (* -3/4 pi)) GreenColor))
    (howto-step "Test")))
  
  (slide/title
   "Guessing a Number"
   (code (code:contract make-secret-checker : num -> (num -> sym))
         (define (make-secret-checker n)
           (local [(define secret (random n))]
             (lambda (m)
               (cond
                 [(= m secret) 'perfect]
                 [(< m secret) 'too-small]
                 [(> m secret) 'too-large])))))
   'next
   (blank)
   (problem "Implement the function" (code discover-number) "which takes a"
            "number" (code _n) "and a function produced by"
            (code (make-secret-checker _n)) ", and returns the secret"
            "number in the function"))
  
  (slide
   rep-step
   (blank)
   (page-para "Apparently done already:")
   (code (code:contract discover-number : num (num -> sym) -> num)))
  
  (slide
   example-step
   (blank)
   (code (discover-number 1 (make-secret-checker 1))
         "should be" 0)
   (blank)
   (code (discover-number 3 (make-secret-checker 3))
         "should be" "0 or 1 or 2"))
  
  (slide
   (hc-append gap-size
              reuse-branch (bt "or")
              template-branch (bt "or")
              genrec-branch (bt "?"))
   (blank)
   (blank)
   'next
   (page-item "Abstract/reuse: nothing obvious")
   'next
   (page-item "Template: nothing for" (code num))
   'next
   (page-para/r "... but is it really" (code nat) "?")
   'next
   (colorize (page-para/r "Yes, starting from 1") BlueColor))
  
  (with-steps
   (init base rest better?)
   (slide
    template-branch
    (blank)
    (vl-append
     line-sep
     (code (code:contract discover-number : nat (nat -> sym) -> nat))
     (lt-superimpose
      ((vbetween init base)
       (code
        (define (discover-number n checker)
          (cond
            [(= n 1) #,(rbl-superimpose
                        ((vonly init) (code ...))
                        ((vafter base) (code 0)))]
            [else 
             ... 
             (discover-number (sub1 n) checker) 
             ...]))))
       ((vafter rest)
        (code
         (define (discover-number n checker)
           (cond
             [(= n 1)   0]
             [else 
              (cond
                [(symbol=? (checker n) 'perfect) n]
                [else
                 (discover-number (sub1 n) checker)])]))))))
    (blank)
    ((vafter better?)
     (colorize
      (page-para* "This works, but is there a better way?")
      BlueColor))))
  
  (define number-line
    (let ([zero (tt "0")])
      (rb-superimpose
       (vl-append
        line-sep
        (inset (hc-append (vline 1 gap-size) (hline (* 3/4 client-w) 1) (vline 1 gap-size))
               (/ (pict-width zero) 2) 0)
        zero)
       (tt "9"))))
  
  (define (red-region number-line start old-start end num res comb)
    (let ([mk
           (lambda (start end num?)
             (let ([lw (* 3/4 client-w)])
               (lb-superimpose
                (lt-superimpose
                 number-line
                 (linewidth
                  9
                  (colorize
                   (if start
                       (inset (hline (* lw (/ (- end start) 9)) 1)
                              (+ (* (/ start 9) lw)
                                 (/ (- (pict-width number-line) lw) 2))
                              (/ gap-size 2) 0 0)
                       (blank))
                   RedColor)))
                ((if num? values ghost)
                 (inset (colorize (tt (number->string num)) RedColor)
                        (* lw (/ num 9)) 0 0 0)))))]
          [ex (lambda (n d)
                (and n (max 0 (min 9 (+ n (* 1/2 d))))))])
      (comb
       (list 'alts
             (list (list (vc-append gap-size (mk (ex old-start -1) (ex end 1) #t) (ghost res)))
                   (list (vc-append gap-size (mk (ex start -1) (ex end 1) #t) res))))
       (mk (ex start -1) (ex end 1) #f))))
  
  (define (lineonly a b) a)
  
  (define save-me #f)
  
  (slide/title
   "Guessing a Number"
   (page-para "If you know a number is between 0 and 9:")
   number-line
   'next
   'alts
   (list (list
          (page-para "and you only get" (code 'perfect) "or" (code 'imperfect)
                     "answers to guesses, there's no better way to find the number")
          'next
          'alts~
          (list (red-region number-line 9 #f 9 9 (code 'imperfect) lineonly)
                (red-region number-line 8 9 9 8 (code 'imperfect) lineonly)
                (red-region number-line 7 8 9 7 (code 'imperfect) lineonly)
                (red-region number-line 6 7 9 6 (code 'imperfect) lineonly)
                (red-region number-line 6 6 9 5 (code 'perfect) lineonly)))
         (list
          (page-para "but you get" (code 'perfect) "," (code 'too-small) ", or" (code 'too-large)
                     "answers, it's better to guess in the middle")
          'next
          'alts~
          (let* ([five+line (red-region number-line 0 #f 4 4 (code 'too-small) cons)]
                 [eight+line (red-region (cdr five+line) 6 #f 9 6 (code 'too-large) cons)]
                 [seven+line (red-region (cdr eight+line) #f #f 9 5 (code 'perfect) cons)])
            (set! save-me (car (cadadr (car seven+line))))
            (list (car five+line)
                  (car eight+line)
                  (car seven+line))))))
  
  (slide
   genrec-branch
   (blank)
   (blank)
   save-me
   (blank)
   (page-item "Trivially solvable if mid-point is" (code 'perfect))
   (page-item "Otherwise, mid-point results cuts the range in half"
              sym:emdash "try again"))

  (define (d-defn scale)
    (scale/improve-new-text
     (code
      (define (discover-number n checker)
        (discover-in-range 0 (sub1 n) checker)))
     scale))
            
  (define (dir-defn scale)
    (scale/improve-new-text
     (code
      (define (discover-in-range lo hi checker)
        (local [(define mid (quotient (+ lo hi) 2))]
          (cond
            [(symbol=? (checker mid) 'prefect) mid]
            [else
             (cond
               [(symbol=? (checker mid) 'too-large)
                (discover-in-range lo mid)]
               [else
                (discover-in-range mid hi)])]))))
     scale))
  
  (with-steps
   (init mid trivial gen gen-cond)
   (slide/title
    "Guessing A Number with Generative Recursion"
    (scale/improve-new-text
     (vl-append
      line-sep
      (d-defn 1.0)
      (code code:blank
            (code:contract discover-in-range : nat nat (nat -> bool) -> num)
            (code:comment "Finds the number between lo and hi (inclusive)"))
      (lt-superimpose
       ((vonly init)
        (code
         (define (discover-in-range lo hi checker)
           (cond
             [_trivial? ...]
             [else 
              ... (discover-in-range ...)
              ... ]))))
       ((vonly mid)
        (code
         (define (discover-in-range lo hi checker)
           (local [(define mid (quotient (+ lo hi) 2))]
             (cond
               [_trivial? ...]
               [else 
                ... (discover-in-range ...)
                ... ])))))
       ((vonly trivial)
        (code
         (define (discover-in-range lo hi checker)
           (local [(define mid (quotient (+ lo hi) 2))]
             (cond
               [(symbol=? (checker mid) 'prefect) mid]
               [else
                ... (discover-in-range ...)
                ... ])))))
       ((vonly gen)
        (code
         (define (discover-in-range lo hi checker)
           (local [(define mid (quotient (+ lo hi) 2))]
             (cond
               [(symbol=? (checker mid) 'prefect) mid]
               [else
                ... (discover-in-range lo mid)
                ... (discover-in-range hi hi) ...])))))
       ((vafter gen-cond)
        (dir-defn 1.0))))
     0.8)))
      
  (define-syntax (xcode stx)
    (syntax-case stx ()
      [(_ e ...) #'(scale/improve-new-text (code e ...) 0.8)]))
  
  (eval-steps
   "Running the Guesser"
   (xcode (discover-number 10 check-7))
   (page-para (htl-append (t "using  ") (d-defn 0.8)))
   (xcode (discover-in-range 0 9 check-7))
   (page-para (htl-append (t "using  ") (dir-defn 0.5)))
   (xcode (cond
            [(symbol=? (check-7 4) 'perfect) 4]
            [else
             (cond
               [(symbol=? (check-7 4) 'too-large)
                (discover-in-range 0 4 check-7)]
               [else
                (discover-in-range 4 9 check-7)])]))
   #f
   (xcode (cond
            [(symbol=? (check-7 4) 'too-large)
             (discover-in-range 0 4 check-7)]
            [else
             (discover-in-range 4 9 check-7)]))
   #f
   (xcode (discover-in-range 4 9 check-7))
   #f
   (xcode (cond
            [(symbol=? (check-7 6) 'perfect) 6]
            [else
             (cond
               [(symbol=? (check-7 6) 'too-large)
                (discover-in-range 4 6 check-7)]
               [else
                (discover-in-range 6 9 check-7)])]))
   #f
   (xcode (discover-in-range 6 9 check-7))
   #f
   (xcode (cond
            [(symbol=? (check-7 7) 'perfect) 7]
            [else
             (cond
               [(symbol=? (check-7 7) 'too-large)
                (discover-in-range 6 7 check-7)]
               [else
                (discover-in-range 7 9 check-7)])]))
   #f
   (xcode 7))
   
  (eval-steps
   "Running the Guesser Again"
   (xcode (discover-number 3 check-2))
   #f
   (xcode (discover-in-range 0 2 check-2))
   #f
   (xcode (cond
            [(symbol=? (check-2 1) 'perfect) 1]
            [else
             (cond
               [(symbol=? (check-2 1) 'too-large)
                (discover-in-range 0 1 check-2)]
               [else
                (discover-in-range 1 2 check-2)])]))
   #f
   (xcode (discover-in-range 1 2 check-2))
   #f
   (xcode (cond
            [(symbol=? (check-2 1) 'perfect) 1]
            [else
             (cond
               [(symbol=? (check-2 1) 'too-small)
                (discover-in-range 1 2 check-7)]
               [else
                (discover-in-range 1 2 check-2)])]))
   #f
   (xcode (discover-in-range 1 2 check-2))
   #f
   (xcode (discover-in-range 1 2 check-2))
   (colorize (page-para "Infinite loop!") RedColor)
   (xcode (discover-in-range 1 2 check-2)))

  (slide/title
   "Generative Recursion and Termination"
   (page-item "With structural recursion, a program always" (dt "terminates"))
   (page-subitem "Every value is finite")
   (blank)
   (page-item "With generative recursion, termination becomes more tricky")
   (page-subitem "You have to argue that the problem size definitely"
                 "gets smaller for every recursive call"))
  
  (slide/title
   "Guessing a Number, Corrected"
    (scale/improve-new-text
     (code
      (define (discover-in-range lo hi checker)
        (local [(define mid (quotient (+ lo hi) 2))]
          (cond
            [(symbol=? (checker mid) 'prefect) mid]
            [else
             (cond
               [(symbol=? (checker mid) 'too-large)
                (discover-in-range lo (sub1 mid))]
               [else
                (discover-in-range (add1 mid) hi)])]))))
     0.8))

  (slide/title
   "Algorithms"
   (page-para "Our" (code discover-in-range) "function is an example of a general" (dt "algorithm")
	      "called" (dt "binary search"))
   'next
   (blank)
   (page-para "Many algorithms are less obvious than binary search")
   (page-para "Mostly you'll use general algorithms, not invent them")
   (page-item "Algorithm textbooks are like \"recipe\" books")
   (page-item "Few people design new general algorithms")
   'next
   (blank)
   (page-para "Generative recursion is far more common than general algorithms,"
	      "and it's often merely structural recursion"))
   

)
