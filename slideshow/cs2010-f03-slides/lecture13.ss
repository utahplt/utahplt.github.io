#cs
(module lecture13 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
	   "utils/eval-step.ss"
           (lib "class.ss"))

  (slide/title
   "Random Numbers"
   (page-para "For HW 6, you need the" (code random) "operator")
   (page-para "It's strange" (symbol 190) "it doesn't return the same result every time for the same input:")
   (code > (random 3)
         0
         > (random 3)
         2
         > (random 3)
         1
         > (random 3)
         2))
  
  (slide/title
   "Random Symbols"
   (page-para "Suppose we need a" (code random-symbol) "function")
   (code > (random-symbol 'huey 'dewey 'louie)
         'dewey
         > (random-symbol 'huey 'dewey 'louie)
         'huey
         > (random-symbol 'huey 'dewey 'louie)
         'dewey
         > (random-symbol 'huey 'dewey 'louie)
         'louie)
   'next
   (page-para "Can we implement it with" (code random) "?"))
  
  (slide/title
   "Random Symbols"
   (code (code:contract random-symbol : sym sym sym -> sym)
         (define (random-symbol a b c)
           (cond
             [(= (random 3) 0) a]
             [(= (random 3) 1) b]
             [(= (random 3) 2) c])))
   'next
   (blank)
   (page-para* "This doesn't work, because" (code random)
               "produces a different result each time"))
  
  (slide/title
   "Saving a Random Number"
   (page-para "On the other hand...")
   (code (define n (random 3))
         (list n n n))
   'next
   (page-para "produces" (code (list 0 0 0))
              "," (code (list 1 1 1))
              ", or" (code (list 2 2 2)))
   (blank)
   (colorize
    (page-para* "Constant definitions name constants, so" (code (random 3))
                "must be evaluted when defining" (code n))
    BlueColor)
   (blank)
   (blank)
   (colorize (it "Try it in the stepper")
             RedColor))
  
  (slide/title
   "A Random Constant"
   (page-para "Does this work?")
   (code (define n (random 3))
         code:blank
         (code:contract random-symbol : sym sym sym -> sym)
         (define (random-symbol a b c)
           (cond
             [(= n 0) a]
             [(= n 1) b]
             [(= n 2) c])))
   'next
   (blank)
   (page-para "Not quite, because it always picks the same symbol")
   'next
   (blank)
   (colorize
    (page-para* "We want" (code (define n (random 3)))
                "that is local to" (code random-symbol) "'s body")
    BlueColor))
  
  (slide/title
   "Local Definitions"
   (page-para "This works, in the" (dt "Intermediate") "language")
   (code (code:contract random-symbol : sym sym sym -> sym)
         (define (random-symbol a b c)
           (local [(define n (random 3))]
             (cond
               [(= n 0) a]
               [(= n 1) b]
               [(= n 2) c]))))
   'next
   (blank)
   (page-item "The" (code local) "form has definitions and a body")
   (page-item "Local definitions are only visible in the body")
   (page-item "Local definitions are evaluated only when the" (code local)
              "is evaluated")
   (page-item "The result of" (code local) "is the result of its body"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (let-syntax ([code (lambda (stx)
                       (syntax-case stx ()
                         [(_ . l)
                          #'(scale/improve-new-text
                             (code . l)
                             0.8 0.8)]))])
    (eval-steps
     "Evaluation with Local"
     (code (define (random-symbol a b c)
             (local [(define n (random 3))]
               (cond
                 [(= n 0) a]
                 [(= n 1) b]
                 [(= n 2) c])))
           (random-symbol 'huey 'dewey 'louie)
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code  (define (random-symbol ...) ...)
            (local [(define n (random 3))]
              (cond
                [(= n 0) 'huey]
                [(= n 1) 'dewey]
                [(= n 2) 'louie]))
            (random-symbol 'huey 'dewey 'louie))
     (page-para "Evaluation of" (code local) "lifts and renames the definition")
     (code (define (random-symbol ...) ...)
           (define n17 (random 3))
           (cond
             [(= n17 0) 'huey]
             [(= n17 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [(= n17 0) 'huey]
             [(= n17 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     (page-para "Evaluation of a constant name finds the value")
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [(= 1 0) 'huey]
             [(= n17 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [false 'huey]
             [(= n17 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [(= n17 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [(= 1 1) 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           (cond
             [true 'dewey]
             [(= n17 2) 'louie])
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code (define (random-symbol ...) ...)
           (define n17 1)
           'dewey
           (random-symbol 'huey 'dewey 'louie))
     #f
     (code  (define (random-symbol ...) ...)
            (define n17 1)
            'dewey
            (local [(define n (random 3))]
              (cond
                [(= n 0) 'huey]
                [(= n 1) 'dewey]
                [(= n 2) 'louie])))
     (page-para "Evaluation of" (code local) "picks a new name each time")
     (code  (define (random-symbol ...) ...)
            (define n17 1)
            'dewey
            (define n45 (random 3))
            (cond
              [(= n45 0) 'huey]
              [(= n45 1) 'dewey]
              [(= n45 2) 'louie]))
     #f
     (code  (define (random-symbol ...) ...)
            (define n17 1)
            'dewey
            (define n45 0)
            (cond
              [(= n45 0) 'huey]
              [(= n45 1) 'dewey]
              [(= n45 2) 'louie]))))
  
  (slide/title/tall
   "Another Example"
   (code (code:contract kind-of-blue? : image -> bool)
         (define (kind-of-blue? i)
           (and 
            (> (total-blue (image->color-list i))
               (total-red (image->color-list i)))
            (> (total-blue (image->color-list i))
               (total-green (image->color-list i))))))
   (page-para "Easier to read, converts image only once:")
   (code (define (kind-of-blue? i)
           (local [(define colors 
                     (image->color-list i))]
             (and (> (total-blue colors)
                     (total-red colors))
                  (> (total-blue colors)
                     (total-green colors)))))))

  (slide/title/tall
   "Another Example"
   (scale/improve-new-text
    (code 
     (define (eat-apples l)
       (cond
         [(empty? l) empty]
         [(cons? l) 
          (cond
            [(symbol=? (first l) 'apple) 
             (eat-apples (rest l))]
            [else 
             (cons (first l) (eat-apples (rest l)))])])))
    0.8 0.8)
   (page-para "Better:")
   (scale/improve-new-text
    (code 
     (define (eat-apples l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define ate-rest (eat-apples (rest l)))]
            (cond
              [(symbol=? (first l) 'apple) ate-rest]
              [else (cons (first l) ate-rest)]))])))
    0.8 0.8))
   
  (slide/title
   "Another Use for Local"
   (page-para (code local) "can define functions as well as constants")
   (page-para "This is useful for making a function private")
   (scale/improve-new-text
    (code (define (random-symbol a b c)
            (local [(define (real-random-symbol a b c)
                      (local [(define n (random 3))]
                        (cond
                          [(= n 0) a]
                          [(= n 1) b]
                          [(= n 2) c])))]
              (cond
                [(and (symbol? a) (symbol? b) (symbol? c))
                 (real-random-symbol a b c)]
                [else (error 'random-symbol "not a symbol")]))))
    0.9 0.9)
   'next
   (blank)
   (colorize (it "Use Check Syntax and mouse over variables")
             RedColor))
  
  (slide/title
   "Lexical Scope"
   (scale/improve-new-text
    (code (define (random-symbol a b c)
            (local [(define (real-random-symbol _a _b _c)
                      (local [(define n (random 3))]
                        (cond
                          [(= n 0) _a]
                          [(= n 1) _b]
                          [(= n 2) _c])))]
              (cond
                [(and (symbol? a) (symbol? b) (symbol? c))
                 (real-random-symbol a b c)]
                [else (error 'random-symbol "not a symbol")]))))
    0.9 0.9)
   (page-para "Italic" (code _a) "could be changed to" (code z)
              "without affecting non-italic" (code a) ", no matter how the code runs")
   (page-para "In other words, bindings are static; this is" (dt "lexical scope")))

  )
