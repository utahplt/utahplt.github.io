#cs
(module lecture2 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/lec1.ss"
	   "utils/lec2.ss"
	   (lib "step.ss" "slideshow")
           (lib "math.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (define outline
    (make-outline
     'reuse "Helper Functions and Reuse" #f
     'cond "Conditionals" #f
     'eval-cond (page-para* (bt "Evaluation Rules for") (code cond)) #f
     'recipe (page-para* (bt "Design Recipe with") (code cond)) #f
     'posn "Compound Data" #f))
  
  (outline 'reuse)
  
  (slide/title
   "Designing Programs"
   (page-para (colorize (bt "Design recipe") BlueColor))
   (page-item "As outlined last lecture")
   'next
   (blank)
   (page-para (colorize (bt "Helper functions and reuse") BlueColor))
   (page-item "Writing writing a function, consider whether existing functions help")
   (page-subitem "Example:" (tt "wearing-glasses?") "uses" (tt "add-glasses"))
   (page-item "Look for  functions that you wish you had written")
   (page-subitem "Example:" (tt "same-person-maybe-disguised?") "needs" (tt "wearing-beard?")))

  (define large-red (box-pict 20 20 "red" #t))
  (define small-blue (box-pict 15 15 "blue" #t))
  
  (with-steps
   (problem contract purpose header examples body reuse fulfill)
   (slide/title
    "Another Example"
    (page-para (rt "Write the function") (tt "bigger-image?") (rt "which checks whether")
               (rt "one image") (rt "has more") (rt "pixels than a second image"))
    (vl-append
     line-sep
     ((vafter contract) (code (code:contract bigger-image? : image image -> bool)))
     ((vafter purpose) (code (code:comment "Returns true if a has more pixels than b")))
     (lt-superimpose
      ((vbetween-excl header body)
       (code (define (bigger-image? a b) ...)))
      ((vbetween-excl body reuse)
       (code (define (bigger-image? a b)
               (> (* (image-width a) (image-height a))
                  (* (image-width b) (image-height b))))))
      ((vafter reuse)
       (code (define (bigger-image? a b)
               (> (image-size a) (image-size b))))))
     (tt " ")
     ((vafter examples)
      (vl-append
       line-sep
       (code (bigger-image? #,large-red #,small-blue) "should be" true)
       (code (bigger-image? #,small-blue #,large-red) "should be" false)))
     (tt " ")
     ((vafter reuse)
      (page-para* (colorize (it "Wish list:") RedColor)
                  (code image-size))))
    ((vafter fulfill)
     (vc-append
      line-sep
      (colorize
       (page-para* "Fullfill wishes by applying the recipe again")
       BlueColor)
      (it "(exercise for the reader)")))))
  
  (with-steps
   (problem cph examples body cond)
   (slide/title
    "Reuse"
    (page-para "We should be able to use" (tt "bigger-image?") "to write the"
               (tt "max-image") "function")
    (vl-append
     line-sep
     ((vafter cph)
      (vl-append
       line-sep
       (code (code:contract max-image : image image -> image))
       (code (code:comment "Returns a if a has more pixels than b,"))
       (code (code:comment "otherwise returns b"))))
     (lt-superimpose
      ((vbetween-excl cph body)
       (code (define (max-image a b) ...)))
      ((vafter body)
       (code (define (max-image a b)
               ... (bigger-image? a b) ...))))
     (tt " ")
     ((vafter examples)
      (vl-append
       line-sep
       (code (max-image #,large-red #,small-blue) "should be" #,large-red)
       (code (max-image #,small-blue #,large-red) "should be" #,large-red))))
    (blank)
    ((vafter cond) (colorize 
		    (page-para* "Instead of returning a" (code bool) ","
				"we need to do one of two things,"
				"so we need" (code cond))
		    BlueColor))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'cond)

  (define (alg-cond l)
     (let ([t (table 2
                     l
                     cc-superimpose cc-superimpose
                     (* 3 gap-size) line-sep)])
       (hc-append gap-size (text "{" 'default (inexact->exact (floor (pict-height t)))) t)))

  (slide/title
   "Conditionals in Algebra"
   (page-para "General format of conditionals in algebra:")
   (alg-cond
    (list (it "answer") (it "question")
          (it "...") (blank)
          (it "answer") (it "question")))
   (page-para "Example:")
   (hc-append (t "abs(x) = ")
              (alg-cond
               (list (t "x") (t "if x > 0")
                     (t "-x") (t "otherwise"))))
   (t "abs(10) = 10")
   (t "abs(-7) = 7"))
  
  (slide/title
   "Conditionals"
   (page-para "General syntax of" (tt "cond") "in our language:")
   (code (cond
           [_question _answer]
           ...
           [_question _answer]))
   (page-item "Any number of" (code cond) "lines")
   (page-item "Each line has one" (it "question") "expression and one"
              (it "answer") "expression")
   'next
   (vl-append
    line-sep
    (code (define (abs x)
            (cond
              [(> x 0) x]
              [else (- x)])))
    (code (abs 10) "should be" 10)
    (code (abs -7) "should be" 7)))

  (slide/title
   "Completing max-image"
   (page-item "Use" (code cond) "to complete" (code max-image))
   (code (define (max-image a b)
           (cond
             [(bigger-image? a b) a]
             [else b]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'eval-cond)
  
  (define (c-step a b c)
    (hc-append gap-size a b c))
  
  (slide/title
   "Evaluation Rules for cond"
   (page-para "First question is literally" (code true) "or" (code else))
   (c-step
    (code (cond
            [true _answer]
            ...
            [_question _answer]))
    sym:rightarrow
    (code _answer))
   (page-item "Keep only the first answer")
   'next
   (colorize (page-para "Example:") BlueColor)
   'alts~
   (list (list (htl-append
                (code (+ 1 (cond
                             [true 1]
                             [false 0])))
                rightarrow
                (code (+ 1 1))
                rightarrow
                (code 2)))
         (list (htl-append
                (code (- 1 (cond
                             [true 0]
                             [(< 10 12) 10]
                             [(>= 10 12) 12])))
                rightarrow
                (code (- 1 0))
                rightarrow
                (code 1)))
         (list (htl-append
                (code (* 1 (cond
                             [true 0])))
                rightarrow
                (code (* 1 0))
                rightarrow
                (code 0)))))
    

  (slide/title
   "Evaluation Rules for cond"
   (page-para "First question is literally" (code false))
   (c-step
    (code (cond
            [false _answer]
            [_question _answer]
            ...
            [_question _answer]))
    sym:rightarrow
    (code (cond
            [_question _answer]
            ...
            [_question _answer])))
   (page-item "Throw away the first line")
   'next
   (colorize (page-para "Example:") BlueColor)
   (vr-append
    gap-size
    (htl-append (code (+ 1 (cond
                             [false 1]
                             [true 17])))
                rightarrow
                (code (+ 1 (cond
                             [true 17]))))
    (htl-append rightarrow
                (code (+ 1 17))
                rightarrow
                (code 18))))
               

  (slide/title
   "Evaluation Rules for cond"
   (page-para "First question isn't a value, yet")
   (c-step
    (code (cond
            [#,(bkbox (code _question) "lightgreen") _answer]
            ...
            [_question _answer]))
    sym:rightarrow
    (code (cond
            [#,(bkbox (code _nextques) "purple") _answer]
            ...
            [_question _answer])))
   (page-para "where" (code _question) rightarrow (code _nextques))
   (page-item "Evaluate first question as sub-expression")
   'next
   (colorize (page-para "Example:") BlueColor)
   (vr-append
    gap-size
    (htl-append (code
                 (+ 1 (cond
                        [(< 1 2) 5]
                        [else 8])))
                rightarrow
                (code (+ 1 (cond
                             [true 5]
                             [else 8]))))
    (htl-append rightarrow
                (code (+ 1 5))
                rightarrow
                (code 6))))
     
  (slide/title
   "Evaluation Rules for cond"
   (page-para "Only queston is false answers")
   (c-step
    (code (cond
	   [false 10]))
    rightarrow
    (colorize (it "error: all questions false") RedColor)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'recipe)
  
  (if condense?
      (skip-slides 1)
      (design-recipe-I 'examples))

  (slide
   (recipe-item "Examples")
   (page-para "When the problem statement divides the input into several categories,"
              "test each one")
   'next
   (blank)
   (colorize (page-para "Example:") BlueColor)
   (page-para (rt "Write the function") (code line-part) (rt "that determines whether")
              (rt "a number is") (rt "on zero, to the left, or to the right")
              (rt "on a number line"))
   (vc-append (cc-superimpose (hc-append (arrowhead (/ gap-size 2) pi)
                                         (hline (/ client-w 2) 2)
                                         (arrowhead (/ gap-size 2) 0))
                              (disk (/ gap-size 2)))
              (text "0" 'modern (/ font-size 2)))
   'next
   (blank)
   (vl-append
    (code (line-part 0) "should be" 'zero)
    (code (line-part -3) "should be" 'left)
    (code (line-part 3) "should be" 'right)))

  (if condense?
      (skip-slides 1)
      (design-recipe-I 'body))
  
  (slide
   (recipe-item "Body")
   (page-para "When the problem statement divides the input into" (it "N") "categories:")
   (page-item "Start the body with a" (code cond) "expression and" (it "N") "lines")
   (page-item "Formulate a question to recognize each category")
   'next
   (blank)
   (colorize (page-para "Example:") BlueColor)
   (page-para (rt "Write the function") (code line-part) (rt "that determines whether")
              (rt "a number is") (rt "on zero, to the left, or to the right")
              (rt "on a number line"))
   'next
   (page-para 
    (htl-append (* 2 gap-size)
                (t "Three cases, so three lines:")                
                (code (define (line-part n)
                        (cond
                          [(= n 0) ...]
                          [(< n 0) ...]
                          [(> n 0) ...]))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'posn)

  (define family (code-align (bitmap "family-small.png")))
  (define ollie (code-align (bitmap "ollie-small.png")))
  (define bradley (code-align (bitmap "btenney.jpg")))
  
  (slide/title/center
   "Finding Images"
   (page-para* (code (image-inside? #,family #,ollie)) rightarrow (code true))
   'next
   (blank)
   (page-para* (code (image-inside? #,family #,bradley)) rightarrow (code false)))
   
  (slide/title
   "Image Tests in Conditionals"
   (page-para "Now we can combine such operators with" (code cond) ":")
   (vl-append
    gap-size
    (code (code:contract detect-person : image image image -> image)
          (code:comment "Returns a or b, depending on which is in i")
          (define (detect-person i a b)
            (cond
              [(image-inside? i a) a]
              [(image-inside? i b) b])))
    (vr-append
     line-sep
     (code (detect-person #,family #,ollie #,bradley))
     (code "should be" #,ollie))))
  
  (slide/title
   "Finding and Adjusting Images"
   (page-para "Suppose we want to write" (code frame-person) ":")
   (vr-append
    gap-size
    (code (frame-person #,family #,ollie))
    (hbl-append gap-size
                (code "should be") 
                (lt-superimpose
                 family
                 (let ([w (pict-width family)]
                       [h (pict-height family)]
                       [ow (pict-width ollie)]
                       [oh (pict-height ollie)]
                       [dx 2]
                       [dy 70])
                   (inset (color-frame (blank ow oh) "white") dx dy 0 0)))))
   'next
   (blank)
   (blank)
   (colorize
    (page-para "Need an operator that reports" (it "where") "an image exists")
    BlueColor))
  
  (define bad-contract
    (code find-image : image image -> num num))
  
  (slide/title
   "Finding an Image Position"
   'alts~
   (list (list bad-contract)
         (list (cc-superimpose
                bad-contract
                (linewidth
                 2
                 (colorize
                  (hline (pict-width bad-contract) (pict-height bad-contract))
                  RedColor)))))
   (colorize
    (bt "Must return a single value")
    RedColor)
   (blank)
   (page-para "Correct contract:")
   (code find-image : image image -> posn)
   (blank)
   (blank)
   (page-item "A" (code posn) "is a" (dt "compound value")))

  (slide/title
   "Positions"
   (page-item posn-def)
   'next
   (blank)
   (colorize (page-para "Examples:") BlueColor)
   (code (make-posn 1 2))
   (code (make-posn 17 0))
   'next
   (blank)
   (page-para "A" (code posn) "is a value, just like a number, symbol, or image"))
  
  (slide/title
   "posn-x and posn-y"
   (page-para "The" (code posn-x) "and" (code posn-y) "operators extract"
              "numbers from a" (code posn) ":")
   (page-para* (code (posn-x (make-posn 1 2))) rightarrow (code 1))
   (page-para* (code (posn-y (make-posn 1 2))) rightarrow (code 2))
   'next
   (blank)
   (page-item "General evaluation rules for any" (code X) "and" (code Y) ":")
   (page-para* (code (posn-x (make-posn X Y))) rightarrow (code X))
   (page-para* (code (posn-y (make-posn X Y))) rightarrow (code Y)))

  (with-steps~
   (ques ans)
   (slide/title
    "Positions and Values"
    (page-para "Is" (code (make-posn 100 200)) "a value?")
    (blank)
    ((vafter ans) (page-para (bt "Yes.")))
    ((vafter ans) posn-def)))

  (with-steps~
   (ques ans rule steps)
   (slide/title
    "Positions and Values"
    (page-para "Is" (code (make-posn (+ 1 2) 200)) "a value?")
    (blank)
    ((vafter ans) (page-para (bt "No.")
                              (code (+ 1 2)) "is not a" (code num) ", yet."))
    (blank)
    ((vafter rule)
     (page-item "Two more evaluation rules:"))
    ((vafter rule)
     (vr-append
      (page-para* (code (make-posn #,(bkbox (code X) "lightgreen") Y))
                  rightarrow
                  (code (make-posn #,(bkbox (code Z) "purple") Y)))
      (page-para* "when" (code X) rightarrow (code Z))))
    ((vafter rule)
     (vr-append
      (page-para* (code (make-posn X #,(bkbox (code Y) "lightgreen")))
                  rightarrow
                  (code (make-posn X #,(bkbox (code Z) "purple"))))
      (page-para* "when" (code Y) rightarrow (code Z))))
    (blank)
    ((vafter steps)
     (colorize (page-para "Example:") BlueColor))
    ((vafter steps)
     (page-para* (code (make-posn (+ 1 2) 200))
                 rightarrow
                 (code (make-posn 3 200))))))
    
     
  (slide/title
   "More Examples"
   (page-para "Try these in DrScheme's stepper:")
   (vl-append
    gap-size
    (code (make-posn (+ 1 2) (+ 3 4)))
    (blank)
    (code (posn-x (make-posn (+ 1 2) (+ 3 4))))
    (blank)
    (code (code:contract pixels-from-corner : posn -> num)
          (define (pixels-from-corner p)
            (+ (posn-x p) (posn-y p)))
          (pixels-from-corner (make-posn 1 2)))
    (blank)
    (code (code:contract flip : posn -> posn)
          (define (flip p)
            (make-posn (posn-y p) (posn-x p)))
          (code:line (flip (make-posn 1 2))))))
  
  )
