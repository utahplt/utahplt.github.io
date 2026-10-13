#cs
(module lecture22 (lib "slideshow.ss" "slideshow")  
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
   "Last Time"
   (hc-append
    (* 3 gap-size)
    (bitmap "infix-calc.png")
    (code
     (define TOTAL 0)
     (define WORKING 0)
     (define PREV-OP +)
     ...
     (define (add-digit n) ...)
     ...
     (define (change-total n OP) ...)
     ...))
   (blank)
   (page-para "The" (code add-digit) "and" (code change-total)
              "functions \"remember\" using" (code TOTAL)
              "," (code WORKING) ", and" (code PREV-OP)))
  
  (slide/title/center
   "Designing Functions with State"
   (page-item "New design tool: organizational charts")
   (page-item (bt "Contract, Purpose, and Header")
              "becomes"
              (bt "Contract, Purpose, Effect, and Header"))
   (page-item "Examples include starting state and effect")
   (page-item "Template includes potential assignments"))
  
  (define (proc name . args)
    (let* ([arg-box (apply cc-superimpose (map ghost args))]
           [input-line (colorize
                        (linewidth
                         2
                         (arrow-line (* 1.5 (pict-width arg-box)) 0 (/ font-size 3)))
                        BlueColor)])
      (let ([args (inset (apply
                          vl-append
                          line-sep
                          (map (lambda (a)
                                 (inset (place-over a a find-lb input-line)
                                        0 0 (/ (pict-width arg-box) 2) 0))
                               args))
                         0 (/ gap-size 3))])
        (inset
         (hc-append args (frame (cc-superimpose 
                                 (ghost args) 
                                 (inset name (/ gap-size 3) (/ gap-size 3)))))
         0 0 (pict-width args) 0))))
  
  (define (state-line l)
    (colorize (linewidth 2 l) RedColor))
  
  (define down-arrow (state-line
                      (inset (arrow-line 0 (* gap-size 2) (/ font-size 3))
                             0 (* gap-size 2) 0 0)))
  (define up-arrow (state-line
                    (inset (arrow-line 0 (- (* gap-size 2)) (/ font-size 3))
                           0 0 0 (* gap-size 2))))
  
  (define (data name tdown? tup? bdown? bup? pad?)
    (inset
     (hc-append
      (/ gap-size 2)
      (rb-superimpose
       (vl-append
        (rt-superimpose
         ((if tup? values ghost) up-arrow)
         (vl-append
          ((if tdown? values ghost) down-arrow)
          (inset (colorize (linewidth 2 (circle (/ font-size 2))) RedColor)
                 (- (/ font-size 9)))))
        ((if bdown? values ghost) down-arrow))
       ((if bup? values ghost) up-arrow))
      name)
     (if pad? (+ (pict-width name) (/ gap-size 2)) 0) 
     0 0 0))
  
  (slide/title
   "Organizational Chart, Effects, Templates"
   (vc-append
    (proc (code add-digit) (code n))
    (data (code WORKING) #t #t #t #t #t)
    (proc (code change-total) (code n) (code OP))
    (hc-append
     gap-size
     (data (code TOTAL) #t #t #f #f #t)
     (data (code PREV-OP) #t #t #f #f #f)))
   'next
   'alts
   (list (list (code (code:contract add-digit : num -> true)
                     (code:comment " Adds a digit to the number being entered")
                     (code:comment " Effect: extends number, updates GUI")
                     (define (add-digit n)
                       ... n 
                       ... WORKING ... (set! WORKING ...) ...)))
         (list (scale/improve-new-text
                (code (code:contract change-total : num (num num -> num) -> true)
                      (code:comment " Combines number and total")
                      (code:comment " Effect: sets total, resets number, sets op, updates GUI")
                      (define (change-total n OP)
                        ... n ... OP
                        ... WORKING ... (set! WORKING ...)
                        ... PREV-OP ... (set! PREV-OP ...)
                        ... TOTAL ... (set! TOTAL ...) ...))
                0.75))))
  (slide/title
   "Examples"
   'alts
   (list (list
          (code (begin
                  (set! WORKING 0)
                  (add-digit 5) "should be" true
                  WORKING "should be" 5)
                (begin
                  (set! WORKING 10)
                  (add-digit 5) "should be" true
                  WORKING "should be" 105)))
         (list
          (code (begin
                  (set! TOTAL 3)
                  (set! WORKING 5)
                  (set! PREV-OP *)
                  (change-total 5 +) "should be" true
                  TOTAL "should be" 15
                  WORKING "should be" 0
                  PREV-OP "should be" +)))))
         
   
  (define-code 
   (one-fish-run one-fish-code)
   (code:comment "The model:")
   (define WEIGHT 3)
   (code:contract feed : num -> num)
   (code:comment " ...")
   (define (feed n)
     (begin
       (set! WEIGHT (+ n WEIGHT))
       WEIGHT))
   code:blank #,(colorize (it " ... tests here ...") comment-color)
   code:blank
   (code:comment "The view:")
   (define msg (make-message (number->string WEIGHT)))
   (code:comment "The control:")
   (define (feed-button n)
     (make-button (string-append "Feed " (number->string n))
                  (lambda (evt)
                    (draw-message
                     msg 
                     (number->string (feed n))))))
   (create-window
    (list (list msg) (list (feed-button 1) (feed-button 3)))))
  
  (define feed-chart
    (vc-append
     (proc (code feed) (code n))
     (data (code WEIGHT) #t #t #f #f #t)))
  
  (define feed-template
    (code (define (feed n)
            ... n ... WEIGHT
            ... (set! WEIGHT ...) ...)))
  
  (slide/title
   "Simpler Example"
   (page-para "Suppose we want a GUI to manage a fish")
   (gray-bitmap "fish-gui.png")
   (mk-run one-fish-run)
   'next
   (blank)
   (blank)
   (page-para "New rule: keep the" (dt "view") "and" (dt "control")
              "separate from the" (dt "model"))
   (page-item "The view and control are in the GUI")
   (page-item "The model is a fish with a weight")
   'next
   (colorize
    (page-para* "Design the model first")
    BlueColor))
  
  (slide/title
   "Fish Model"
   (page-item "The only operation in the model is" (code feed))
   (code (code:contract feed : num -> num)
         (code:comment " Grows the fish by n, returns new size")
         (code:comment " Effect: adjusts the fish's weight"))
   'next
   'alts
   (list (list (hc-append (* 3 gap-size) feed-chart (ghost feed-template)))
         (list (hc-append (* 3 gap-size) feed-chart feed-template)))
   'next
   (code
    (begin
      (set! WEIGHT 1)
      (feed 10) "should be" 11
      WEIGHT "should be" 11)))
  
  (define (mvc-box s)
    (frame (inset (t s) (/ gap-size 2))))
  (define double-arrows
    (let ([w (* 3 gap-size)]
          [h/2 (/ font-size 4)])
      (vc-append
       (inset (arrow-line w 0 (/ font-size 3)) 0 h/2 w h/2)
       (inset (arrow-line (- w) 0 (/ font-size 3)) w h/2 0 h/2))))

  (slide/title/center
   "Fish Model Implementation"
   (code (code:contract feed : num -> num)
         (code:comment " Grows the fish by n, returns new size")
         (code:comment " Effect: adjusts the fish's weight")
         (define (feed n)
           (begin
             (set! WEIGHT (+ WEIGHT n))
             WEIGHT))
         code:blank
         (begin
           (set! WEIGHT 1)
           (feed 10) "should be" 11
           WEIGHT "should be" 11)))
         
   

  (slide/title/center
   "Implementing the View and Controller"
   (gray-bitmap "fish-gui.png")
   (page-para "Use the GUI teachpack to construct view and control")
   (page-item "Message objects implement the view")
   (page-item "Button callbacks implement the control")
   (hc-append (mvc-box "View")
              double-arrows
              (mvc-box "Control")
              double-arrows
              (mvc-box "Model"))
   (blank)
   (page-para "Often, the model never calls the control"))
  
  (slide/title/tall
   "Complete Fish Program"
   one-fish-code)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (ecode stx)
    (syntax-case stx ()
      [(_  . rest)
       #'(scale/improve-new-text (code . rest) 0.8)]))

  (define mk-fish-defn
    (ecode (define (make-fish init-weight)
             (local [(define WEIGHT init-weight)
                     (define (feed n)
                       (begin 
                         (set! WEIGHT (+ WEIGHT n))
                         WEIGHT))
                     ...]
               (create-window ...)))))

  (slide/title
   "Multiple Fish"
   (page-para "As we saw last time, if we want multiple fish, we can"
              "use" (code local))
   mk-fish-defn)
   
  (eval-steps
   "Evaluating make-fish"
   (vl-append
    line-sep
    mk-fish-defn
    (ecode (make-fish 5)))
   #f
   (ecode ...
          (local [(define WEIGHT 5)
                  (define (feed n)
                    (begin 
                      (set! WEIGHT (+ WEIGHT n))
                      WEIGHT))
                  ...]
            (create-window ...)))
   #f
   (ecode ...
          (define WEIGHT_65 5)
          (define (feed_67 n)
            (begin 
              (set! WEIGHT_65 (+ WEIGHT_65 n))
              WEIGHT_65))
          ...
          (create-window ...)))
  
  (slide/title
   "Multiple Fish"
   (page-para "Every time we call" (code make-fish)
              "a new" (code WEIGHT) "is created for the new fish")
   (page-para "We can make a whole aquarium....")
   'next
   (blank)
   (page-item "How can we get the current total weight of all fish?")
   (page-item "How can we auto-feed all fish?")
   'next
   (colorize
    (page-para* "Problem:" (code make-fish) "returns only a window")
    RedColor)
   (colorize
    (page-para* "The renamed" (code WEIGHT) "is completely hidden")
    BlueColor))
  
  (slide/title
   "Returning the Weight"
   (page-para "Does this help?")
   (code (code:contract make-fish : num -> num)
         (define (make-fish init-weight)
           (local [(define WEIGHT init-weight)
                   ...]
             (begin 
               (create-window ...)
               WEIGHT))))
   'next
   (page-para (bt "No:"))
   (vl-append
    line-sep
    (page-para* (code (make-fish 5)))
    (page-para* sym:rightarrow (code (local [(define WEIGHT 5) ...] ... WEIGHT)))
    (page-para* sym:rightarrow (code (define WEIGHT_73 5) ... WEIGHT_73))
    (page-para* sym:rightarrow sym:rightarrow (code (define WEIGHT_73 5) ... 5))))
  
  (slide/title
   "Returning the Feeder"
   (page-para "Only" (it "functions") "inside" (code make-fish)
              "can see" (code WEIGHT))
   'next
   (page-para "So maybe" (code make-fish) "should return a function:")
   (code (code:contract make-fish : num -> (num -> num))
         (define (make-fish init-weight)
           (local [(define WEIGHT init-weight)
                   (define (feed n) ... WEIGHT ...)
                   ...]
             (begin 
               (create-window ...)
               feed))))
   (blank)
   (vl-append
    line-sep
    (page-para* (scode (make-fish 5)))
    (htl-append sym:rightarrow (t " ")
                (scode (local [(define WEIGHT 5) (define (feed n) ... WEIGHT ...) ...]
                         ... feed)))
    (page-para* sym:rightarrow (scode (define WEIGHT_77 5) (define (feed_81 n) ... WEIGHT_77 ...) ... feed_81))))
   
  (slide/title
   "Feeding an Aquarium"
   (scale/improve-new-text
    (code (code:comment "A live-fish is")
          (code:comment "  (num -> num)")
          code:blank
          (code:contract make-fish : num -> live-fish)
          ...
          (define aquarium (list (make-fish 5) 
                                 (make-fish 3)
                                 (make-fish 12)))
          code:blank
          (code:contract aq-weight : list-of-live-fish -> num)
          (define (aq-weight l)
            (foldr (lambda (f r) (+ (f 0) r)) 0 l))
          code:blank
          (code:contract feed-all : n list-of-live-fish -> ...)
          (define (feed-all n l)
            (map (lambda (f) (f n)) l)))
    0.9))

  (define feed-all!-defn
    (scale/improve-new-text
     (code
      (code:contract feed-all! : n list-of-live-fish -> (void))
      (code:comment " Feeds n to each live-fish in l")
      (code:comment " Effect: each live-fish becomes heavier")
      (define (feed-all! n l)
        (for-each (lambda (f) (f n)) l)))
     0.9))
  
  (with-steps
   (basic bad-test bad-test-expl good-test good-test-note)
   (slide/title
    "for-each"
    (page-para "The built-in function" (code for-each)
               "is like" (code map) ", but it returns" (code (void)))
    (vl-append
     line-sep
     feed-all!-defn
     (lt-superimpose
      ((vbetween-excl bad-test good-test)
       (vr-append
        (scale/improve-new-text
         (code
          code:blank
          (begin
            (define l (list (make-fish 1) (make-fish 2)))
            (feed-all! 3 l) "should be" (void)
            l "should be" (list (make-fish 4) (make-fish 5))))
         0.9)
        (colorize (bt "?") RedColor)))
      ((vafter good-test)
       (scale/improve-new-text
        (code
         code:blank
         (begin
           (define l (list (make-fish 1) (make-fish 2)))
           (feed-all! 3 l) "should be" (void)
           ((first l) 0) "should be" 4
           ((first (rest l)) 0) "should be" 5))
        0.9))))
    (blank)
    (lt-superimpose
     ((vonly bad-test-expl) (page-item "This test doesn't completely capture the effect"))
     ((vafter good-test-note)
      (vl-append
       gap-size
       (page-item "Testing with state is often difficult")
       (page-item "Avoid this difficulty by avoiding state"))))))
  
  (slide/title
   "A Tale of Two Fish Representations"
   (code (code:comment "A fish is")
         (code:comment "  num")
         code:blank
         (code:comment "A live-fish is")
         (code:comment "  (num -> num)"))
   (blank)
   'alts
   (list
    (list
     (page-item "A" (code fish) "represents a fish of a particular weight")
     (page-subitem "Feed the fish" sym:implies "new value")
     (page-item "A" (code live-fish) "represents a fish with a particular identity")
     (page-subitem "Feed the fish" sym:implies "same value, new state"))
    (list
     (page-para (code live-fish) "is more closely reflects reality")
     (page-item "On the one hand, reflecting reality makes things more intuitive")
     (page-item "On the other hand, reality can be messy")
     (blank)
     (colorize
      (page-para* "Key question when designing a program: what to represent")
      BlueColor))))
  
  (slide/title
   "Encapsulation"
   (page-para "Packaging fish state with its operations is called" (dt "encapsulation"))
   (page-para "More on encapsulation soon..."))

  (slide/title
   "Design with State Summary"
   (page-item "Deciding to use state: often motivated by GUIs")
   (page-subitem "Split into model and view/controller")
   (blank)
   (page-item "The design recipe for state")
   (page-subitem "Charts (no handin artifact)")
   (page-subitem "Effects (handin with purpose)")
   (page-subitem "Template with assignments (handin optional)")
   (page-subitem "Multi-step tests (handin as usual)")
   (blank)
   (page-item "Design for the single-instance case, then encapsulate if necessary"))
  
  
  )
