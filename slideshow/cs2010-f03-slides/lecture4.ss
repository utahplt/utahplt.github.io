#cs
(module lecture4 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
	   (lib "step.ss" "slideshow")
           (lib "math.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (define ant-defn
    (code (code:comment "An ant is")
          (code:comment " (make-ant num posn)")))

  (define posn-defn
    (code (code:comment "A posn is")
          (code:comment " (make-posn num num)")))

  (slide/title
   "Expanding the Zoo"
   (page-para "We have snakes and armadillos. Let's add ants.")
   (page-para "An ant has")
   (page-item "a weight")
   (page-item "a location in the zoo")
   'next
   (blank)
   (vl-append
    line-sep
    ant-defn
    (code (define-struct ant (weight loc))))
   'next
   (blank)
   (code (make-ant 0.001 (make-posn 4 5)))
   (code (make-ant 0.007 (make-posn 3 17))))

  (slide/title
   "Programming with Ants"
   (problem "Define" (code ant-at-home?) ", which takes"
            "an ant and reports whether it is at the origin"))

  (define ant-tmpl-code
    (code (define (ant-at-home? a)
            ... (ant-weight a)
            ... (posn-at-home? (ant-loc a)) ...)))
  
  (define ant-tmpl
    (code (code:template (define (ant-at-home? a)
                           ... (ant-weight a)
                           ... (posn-at-home? (ant-loc a)) ...))))

  (define posn-tmpl-code
    (code (define (posn-at-home? p)
            ... (posn-x p) ... (posn-y p) ...)))

  (define posn-tmpl
    (code (code:template (define (posn-at-home? p)
                           ... (posn-x p) ... (posn-y p) ...))))
    
  (with-steps
   (contract purpose header examples template template2 template3 body)
   (slide/title/tall
    "Programming with Ants"
    (cc-superimpose
     ((vbetween contract header) (recipe-item "Contract, Purpose, and Header"))
     ((vbetween examples examples) (recipe-item "Examples"))
     ((vbetween template template3) (recipe-item "Template"))
     ((vafter body) (recipe-item "Body")))
    (vc-append
     gap-size
     (vl-append
      line-sep
      (code (code:contract ant-at-home? : ant -> bool))
      ((vafter purpose) (code (code:comment "Check whether ant a is home")))
      (lt-superimpose
       ((vbetween-excl header template) (code (define (ant-at-home? a)
                                                  ...)))
       ((vbetween template template) 
        (code (define (ant-at-home? a)
                ... (ant-weight a)
                ... (ant-loc a) ...)))
       ((vbetween template2 template2) 
        (vl-append
         line-sep
         ant-tmpl-code
         (tt " ")
         (colorize (page-para* "New template rule: data-defn reference" sym:implies "template reference")
                   RedColor)
         (tt " ")
         (colorize (page-para* "Add templates for referenced data, if needed, and") BlueColor)
         (colorize (page-para* "implement body for referenced data") BlueColor)))
       ((vbetween template3 template3)
	(vl-append
         line-sep
         ant-tmpl-code
         (tt " ")
	 posn-tmpl-code))
       ((vafter body) 
        (vl-append
         line-sep
         ant-tmpl
         posn-tmpl
         (code (define (ant-at-home? a)
                 (posn-at-home? (ant-loc a)))
               (define (posn-at-home? p)
                 (and (= (posn-x p) 0) (= (posn-y p) 0)))))))
      (tt " ")
      ((vafter examples)
       (scale/improve-new-text
	(vl-append
	 line-sep
	 (code (ant-at-home? (make-ant 0.001 (make-posn 0 0))) '= true)
	 (code (ant-at-home? (make-ant 0.001 (make-posn 1 1))) '= false))
	0.8))))))
  
  (slide/title
   "Shapes of Data and Templates"
   (bkbox (page-para* "The shape of the template matches the shape of the data")
          "yellow")
   
   (add-gp-arrow
    (vl-append
     ant-defn
     (t " ")
     posn-defn)
    4/5 2/5 1/3 35/50)
   
   (add-gp-arrow
    (vl-append
     line-sep
     ant-tmpl-code
     (t " ")
     posn-tmpl-code)
    1/2 3/7 1/2 5/7))
   

  (slide/title
   "Programming with Ants"

   (problem "Define" (code feed-ant) ", which feeds"
            "an ant 0.001 lbs of food")
   (problem "Define" (code move-ant) ", which takes"
            "an ant, an amount to move X, and an amount to move Y, "
	    "and returns a moved ant"))
   
  (slide/title
   "Animals"
   (page-para "All animals need to eat...")
   (problem "Define" (code feed-animal) ", which takes"
            "an animal (snake, dillo, or ant) and feeds it"
            "(5 lbs, 2 lbs, or 0.001 lbs, respectively)")
   'next
   (blank)
   (colorize (page-para* "What is an" (code animal) "?") BlueColor))
   
  (define animal-defn
    (code (code:comment "An animal is either")
          (code:comment " - snake")
          (code:comment " - dillo")
          (code:comment " - ant")))
  
  (slide/title
   "Animal Data Definition"
   animal-defn
   'next
   (blank)
   (page-para "The \"either\" above makes this a new kind of data definition:")
   (page-para* "data with" (dt "varieties"))
   'next
   (blank)
   (colorize (page-para "Examples:") BlueColor)
   (code (make-snake 'slinky 10 'rats))
   (code (make-dillo 2 true))
   (code (make-ant 0.002 (make-posn 3 4))))

  (define feed-animal-examples
    (vl-append
     line-sep
     (code (feed-animal (make-snake 'slinky 10 'rats)))
     (code "should be" (make-snake 'slinky 15 'rats))
     (tt " ")
     (code (feed-animal (make-dillo 2 true)))
     (code "should be" (make-dillo 4 true))
     (tt " ")
     (code (feed-animal (make-ant 0.002 (make-posn 3 4))))
     (code "should be" (make-ant 0.003 (make-posn 3 4)))))

  (slide/title
   "Feeding Animals"
   (page-para
    (vl-append
     line-sep
     (code (code:contract feed-animal : animal -> animal))
     (code (code:comment "To feed the animal a"))
     (code (define (feed-animal a)
             ...))))
   'next
   (blank)
   (page-para
    feed-animal-examples))
    
  (slide/title
   "Template for Animals"
   (page-para "For the template step...")
   (code (define (feed-animal a)
           ...))
   (page-item "Is" (code a) "compound data?")
   'next
   (blank)
   (page-item "Technically yes, but the definition" (code animal) "doesn't have"
              (hbl-append (code make-) (it "something")) 
              ", so we don't use the compound-data template rule"))

  (define animal-half-tmpl
    (code (define (feed-animal a)
            (cond
              [... ...]
              [... ...]
              [... ...]))))
    
  (slide/title
   "Template for Varieties"
   (page-para "Choice in the data definition")
   animal-defn
   (page-para "means" (code cond) "in the template:")
   animal-half-tmpl
   (blank)
   (colorize
    (page-para* "Three data choices means three" (code cond) "cases")
    BlueColor))
  
  (slide/title
   "Questions for Varieties"
   animal-half-tmpl
   (page-para "How do we write a question for each case?")
   'next
   (blank)
   (vc-append
    line-sep
    (page-para "It turns out that")
    (page-para* (code (define-struct snake (name weight food))))
    (page-para "provides" (code snake?)))
   (blank)
   (vl-append
    line-sep
    (page-para* (code (snake? (make-snake 'slinky 5 'rats))) rightarrow (code true))
    (page-para* (code (snake? (make-dillo 2 true))) rightarrow (code false))
    (page-para* (code (snake? 17)) rightarrow (code false))))

  (define animal-3/4-tmpl
    (code (define (feed-animal a)
            (cond
              [(snake? a) ...]
              [(dillo? a) ...]
              [(ant? a) ...]))))
    
  (slide
   (recipe-item "Template")
   animal-3/4-tmpl
   (colorize (page-para* "New template rule: varieties" sym:implies (code cond))
             RedColor)
   'next
   (blank)
   (page-para "Now continue template case-by-case...")) 
  
  (define animal-tmpl
    (code (define (feed-animal a)
            (cond
              [(snake? a) ... (feed-snake a) ...]
              [(dillo? a) ... (feed-dillo a) ...]
              [(ant? a) ... (feed-ant a) ...]))))
    
  (slide
   (recipe-item "Template")
   animal-tmpl
   (blank)
   (page-para "Remember: references in the data definition" sym:implies "template references")
   'next
   animal-defn)
  
  (define all-defns
    (let* ([p (vl-append
               line-sep
               animal-defn
               (code code:blank
                     (code:comment "A snake is")
                     (code:comment "(make-snake sym num sym)")
                     code:blank
                     (code:comment "A dillo is")
                     (code:comment "(make-dillo num bool)")
                     code:blank
                     (code:comment "An ant is")
                     (code:comment "(make-ant num posn)")
                     code:blank
                     (code:comment "A posn is")
                     (code:comment "(make-posn num num)")))]
           [up (/ 1/16 2)]
           [a->s (add-gp-arrow p 1/3 (- 2/16 up) 1/3 5/16)]
           [a->d (add-gp-arrow a->s 1/3 (- 3/16 up) 1/3 8/16)]
           [a->a (add-gp-arrow a->d 1/3 (- 4/16 up) 1/3 11/16)]
           [a->p (add-gp-arrow a->a 2/3 (- 13/16 up) 1/3 14/16)])
      a->p))
  
  (define all-tmpls
    (let* ([p (vl-append
               line-sep
               animal-tmpl
               (code code:blank
                     (define (feed-snake s)
                       ... (snake-name s) ... (snake-weight s)
                       ... (snake-food s) ...)
                     code:blank
                     (define (feed-dillo d)
                       ... (dillo-weight d) 
                       ... (dillo-alive? d) ...)
                     code:blank
                     (define (feed-ant a)
                       ... (ant-weight d) 
                       ... (feed-posn (ant-loc d)) ...)
                     code:blank
                     (define (feed-posn p)
                       ... (posn-x p) ... (posn-y p) ...)))]
           [up (/ 1/20 2)]
           [a->s (add-gp-arrow p 3/5 (- 3/20 up) 1/3 6/20)]
           [a->d (add-gp-arrow a->s 3/5 (- 4/20 up) 1/3 10/20)]
           [a->a (add-gp-arrow a->d 3/5 (- 5/20 up) 1/3 14/20)]
           [a->p (add-gp-arrow a->a 1/3 (- 17/20 up) 1/3 18/20)])
      a->p))
  
  (slide/title
   "Shapes of Data and Templates"
   (lt-superimpose
    (scale all-defns 0.65 0.65)
    (rt-superimpose
     titleless-page
     (scale all-tmpls 0.65 0.65))))

  (design-recipe-III (if condense? #f 'data))
  (slide
   (recipe-item "Data")
   (page-para "When the problem statement mentions" (code N) "different varieties"
              "of a thing, write a data definition of the form")
   (vl-append
    line-sep
    (code (code:comment "A thing is"))
    (code (code:comment " - variety1"))
    (code (code:comment " ..."))
    (code (code:comment " - varietyN"))))
  
  (if condense?
      (skip-slides 1)
      (design-recipe-III 'examples))
  (slide
   (recipe-item "Examples")
   (page-para "When the input data has varieties, be sure to pick each variety at least once.")
   animal-defn
   feed-animal-examples)

  (if condense?
      (skip-slides 1)
      (design-recipe-III 'template))
  (slide
   (recipe-item "Template")
   (page-para "When the input data has varieties, start with" (code cond))
   (page-item (code N) "varieties" sym:implies (code N) (code cond) "lines")
   (page-item "Formulate a question to match each corresponding variety")
   (page-item "Continue template steps case-by-case")
   (blank)
   'alts
   (list (list animal-3/4-tmpl)
         (list
          (page-para "When the data definition refers to a data definition,"
                     "make the template refer to a template")
          (blank)
          'alts
          (list (list (vl-append
                       ant-tmpl-code
                       (tt " ")
                       posn-tmpl-code))
                (list animal-tmpl)))))
  
  )
