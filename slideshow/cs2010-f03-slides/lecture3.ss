#cs
(module lecture3 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
	   (lib "step.ss" "slideshow")
           (lib "math.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (define outline
    (make-outline
     'posn (code posn) #f
     'define-struct (code define-struct) #f))
  
  (outline 'posn)
  
  (define posn-def
    (vl-append
     gap-size
     (page-para* "A" (code posn) "is")
     (page-para* (ghost bullet) (code (make-posn num num)))))
  
  (slide/title
   "Compound Data So Far"
   posn-def
   (blank)
   (page-item (code (make-posn 1 2)) "is a value")
   (page-item (code (posn-x (make-posn 1 2))) rightarrow (code 1))
   (page-item (code (posn-y (make-posn 1 2))) rightarrow (code 2))
   'next
   (blank)
   (blank)
   (page-para "So much for computation... how about program design?"))
  
  (if condense?
      (skip-slides 1)
      (design-recipe-I 'body))
  
  (define template-todo
    (page-para "If the input is compound data, start the"
               "body by selecting the parts"))
    
  (define c
    (code (code:contract max-part : posn -> num)))
    
  (define c&p
    (vl-append
     line-sep
     c
     (code (code:comment "Return the X part of p is it's bigger"))
     (code (code:comment "than the Y part, otherwise the Y part"))))

  (define examples
    (vl-append
     line-sep
     (code (max-part (make-posn 10 11)) "should be" 11)
     (code (max-part (make-posn 7 5)) "should be" 7)))
  
  (define tmpl-body
    (code (define (max-part p)
            ... (posn-x p) ... (posn-y p) ...)))
  
  (with-steps
   (step cphe body0 body template)
   (slide
    (recipe-item "Body")
    template-todo
    ((vafter cphe)
     (vl-append
      line-sep
      c&p
      (lt-superimpose
       ((vbetween-excl cphe body0) 
        (code (define (max-part p)
                ...)))
       ((vbetween-excl body0 body) 
        tmpl-body)
       ((vafter body) 
        (code (define (max-part p)
                (cond
                  [(> (posn-x p) (posn-y p)) (posn-x p)]
                  [else (posn-y p)])))))
      examples))
    (blank)
    ((vafter template)
     (colorize
      (page-para "Since this guideline applies before the usual body work,"
                 "let's split it into an explicit step")
      BlueColor))))
   
  (design-recipe-II 'template)

  (slide
   (recipe-item (hc-append (strike-through (bt "Body")) (bt " Template")))
   template-todo
   'alts
   (list
    (list (vl-append
           line-sep
           c
           (code (code:comment "..."))
           tmpl-body)
          'next
          (blank)
          (page-para
           (htl-append (colorize (t "Check:") GreenColor)
                       (t " ")
                       (vl-append
                        line-sep
                        (t "number of parts in template =")
                        (t "number of parts data definition named in contract"))))
          'next
          (blank)
          posn-def)
    (list
     (page-para (colorize (t "Handin artifact:") RedColor) "a comment (required starting with HW 3)")
     (vl-append
      line-sep
      c&p
      (code (code:template (define (max-part p)
                             ... (posn-x p) ... (posn-y p) ...)))
      tmpl-body
      examples))))

  (if condense?
      (skip-slides 1)
      (design-recipe-II #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'define-struct)
  
  (slide/title
   "Other Kinds of Data"
   (page-para "Suppose we want to represent snakes:")
   (vl-append
    gap-size
    (page-item* "name")
    (page-item* "weight")
    (page-item* "favorite food"))
   (page-para "What kind of data is appropriate?")
   'next
   (blank)
   (colorize
    (page-para* "Not" (code num) "," (code bool) "," (code sym) "," (code image) ", or" (code posn) "...")
    BlueColor))
  
  (slide/title
   "Data Definitions and define-struct"
   (page-para "Here's what we'd like:")
   (vl-append
    line-sep
    (page-para* "A" (code snake) "is")
    (page-para* (ghost bullet) (code (make-snake sym num sym))))
   'next
   (colorize
    (page-para* "But" (code make-snake) "is not built into DrScheme")
    BlueColor)
   'next
   (blank)
   (page-para "We can tell DrScheme about" (code snake) ":")
   (code (define-struct snake (name weight food)))
   'next
   (page-para "Creates the following:")
   'alts
   (list (list (vl-append
                line-sep
                (page-item* (code make-snake))
                (page-item* (code snake-name))
                (page-item* (code snake-weight))
                (page-item* (code snake-food))))
         (list
          (vl-append
           line-sep
           (page-para* (code (snake-name (make-snake X Y Z))) rightarrow (code X))
           (page-para* (code (snake-weight (make-snake X Y Z))) rightarrow (code Y))
           (page-para* (code (snake-food (make-snake X Y Z))) rightarrow (code Z))))))
  
  (slide
   (recipe-item "Data")
   (page-para "Deciding to define" (code snake) "is in the first step of the design recipe")
   'next
   (blank)
   (page-para (colorize (t "Handin artifact:") RedColor) "a comment and/or" (code define-struct))
   (vl-append
    line-sep
    (code (code:comment "A snake is"))
    (code (code:comment "  (make-snake sym num sym)"))
    (tt " ")
    (code (define-struct snake (name weight food))))
   'next
   (blank)
   (page-para "Now that we've defined" (code snake) ", we can use it in contracts"))

  (slide/title
   "Programming with Snakes"
   (blank)
   (blank)
   (problem "Implement" (code snake-skinny?) ", which takes a snake and returns"
            (code true) "if the snake weights less than 10 pounds,"
            (code false) "otherwise")
   'next
   (problem "Implement" (code feed-snake) ", which takes a snake and returns"
            "a snake with the same name and favorite food,"
            "but five pounds heavier"))
  
  (slide/title
   "Programming with Armadillos"
   (blank)
   (problem "Pick a representation for armadillos (\"dillo\" for short),"
            "where a dillo has a weight and may or may not be alive")
   'next
   (problem "Implement" (code run-over-with-car) ", which takes a dillo"
            "and returns a dead dillo of equal weight")
   'next
   (problem "Implement" (code feed-dillo) ", where a dillo eats 2 pounds of"
            "food at a time")
   'next
   (page-para/r (rt "... unless it's dead")))
    
  )



