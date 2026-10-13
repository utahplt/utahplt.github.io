#cs
(module lecture7 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
	   (lib "step.ss" "slideshow")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (define orig-fish
    (standard-fish 150 75 'right "red" "black"))
  
  (define orig-two-fish
    (hc-append orig-fish
               (inset (standard-fish 100 50 'left "blue" "black")
                      gap-size 50 0 0)))

  (define third-fish 
    (standard-fish 120 50 'left "green" "black"))

  (define (make-three-fish two-fish third-fish)
    (vc-append
     (inset two-fish gap-size 0 0 0)
     third-fish))
  
  (define one-fish (make-three-fish (cc-superimpose orig-fish (ghost orig-two-fish))
                                    (ghost third-fish)))
  (define two-fish (make-three-fish orig-two-fish (ghost third-fish)))
  (define three-fish (make-three-fish orig-two-fish third-fish))

  (define aq-defn
    (code (code:comment "An aquarium is")
          (code:comment " (make-aq num num)")))
         
  (slide/title
   "Aquarium"
   (page-para "Our zoo was so successful, let's start an aquarium")
   (blank)
   (in-aq two-fish)
   (blank)
   (page-para "For a fish, we only care about its weight, so for two fish:")
   (vl-append line-sep
              aq-defn
              (code (define-struct aq (first second)))))
   
  (slide/title
   "Aquarium Template"
   (vl-append
    line-sep
    aq-defn
    (tt " ")
    (t "Generic template:")
    (code (code:contract func-for-aq : aquarium -> ...)
          (code:template (define (func-for-aq a)
                           ... (aq-first a) ... (aq-second a) ...))))
   'next
   (blank)
   (blank)
   (code (code:contract aq-weight : aquarium -> num)
         (define (aq-weight a)
           (+ (aq-first a) (aq-second a)))
         code:blank
         (aq-weight (make-aq 7 8)) "should be" 15)
   'next
   (blank)
   (page-para "And so on, for many other simple aquarium functions..."))

  (define one-aq-defn
    (code (code:comment "An aquarium is")
          (code:comment " (make-aq num)")))
  
  (slide/title
   "Tragedy Strikes the Aquarium"
   (page-para "Poor blue fish... now we have only one")
   (blank)
   (in-aq one-fish)
   (blank)
   'next
   (page-para "Worse, we have to re-write all our functions...")
   (vl-append line-sep
              one-aq-defn
              (code (define-struct aq (first)))))
  
  (slide/title
   "Aquarium Template, Revised"
   (vl-append
    line-sep
    one-aq-defn
    (tt " ")
    (code (code:contract func-for-aq : aquarium -> ...)
          (code:template (define (func-for-aq a)
                           ... (aq-first a) ...))))
   'next
   (blank)
   (blank)
   (code (code:contract aq-weight : aquarium -> num)
         (define (aq-weight a)
           (aq-first a))
         code:blank
         (aq-weight (make-aq 7)) "should be" 7)
   'next
   (blank)
   (page-para "And so on, for" (bt "all") "of the aquarium functions..."))

  (slide/title
   "The Aquarium Expands"
   (page-para "Hooray, we have two new fish!")
   (blank)
   (in-aq three-fish)
   (blank)
   'next
   (page-para "Unfortunately, we have to re-re-write all our functions...")
   (code (code:comment "An aquarium is")
         (code:comment " (make-aq num num num)")
         (define-struct aq (first second third))))
  
  (slide/title/center
   "A Flexible Aquarium Representation"
   (page-para "Our data choice isn't working")
   (page-item "An aquarium isn't just 1 fish, 2 fish, or 100 fish" 
              (symbol 190)
              "it's a collection"
              "containing an arbitrary number of fish")
   (page-item "No data definition with just 1, 2, or 100 numbers will work")
   (blank)
   (blank)
   (blank)
   (page-para "To represent an aquarium, we need a" (dt "list") "of numbers")
   (colorize
    (page-para* "We don't need anything new in the language, just a new idea")
    BlueColor))
   
  (define empty-slot (blank gap-size))

  (define (box-em . l)
    (let ([max-h (+ (apply max (map pict-height l)) gap-size)])
      (apply hc-append
             (map (lambda (p)
                    (linewidth
                     2
                     (frame (cc-superimpose (blank (+ (pict-width p) gap-size) max-h) 
                                            p))))
                  l))))
  
  (slide/title
   "Structs as Boxes"
   (page-para "Pictorially,")
   (page-item (code define-struct) "lets us define a new kind of box")
   (page-item "The box can have as many compartments as we want,"
              "but we have to pick how many, once and for all")
   (blank)
   (code (define-struct snake (name weight food)))
   (hc-append gap-size sym:implies (box-em empty-slot empty-slot empty-slot))
   (blank)
   (code (define-struct ant (weight loc)))
   (hc-append gap-size sym:implies (box-em empty-slot empty-slot)))

  (slide/title/center
   "Boxes Stretch"
   (page-para "The boxes stretch to fit any one thing in each slot:")
   (box-em (code 'slinky) (code 12) (code 'rats))
   (page-para "Even other boxes:")
   (box-em (code 0.002) (box-em (code 2) (code 3)))
   (blank)
   (colorize (page-para* "Still, the number of slots is fixed") BlueColor))
  
  (define (small-fish color)
    (standard-fish 75 50 'right color "black"))

  (slide/title
   "Packing Boxes"
   (page-para "Suppose that")
   (page-item "You have four things to pack as one")
   (page-item "You only have 2-slot boxes")
   (page-item "Every slot must contain exactly one thing")
   (colorize (page-para* "How can you create a single package?") BlueColor)
   (blank)
   (hc-append
    (* 3 gap-size)
    (table 2 (list (small-fish "red") (small-fish "blue")
                   (small-fish "green") (small-fish "purple"))
           cc-superimpose cc-superimpose
           gap-size gap-size)
    (vc-append
     gap-size
     (box-em empty-slot empty-slot)
     (box-em empty-slot empty-slot)
     (box-em empty-slot empty-slot)
     (box-em empty-slot empty-slot))))

  (define pack-two-fish-1 (box-em (small-fish "red") (small-fish "blue")))
  (define pack-two-fish-2 (box-em (small-fish "green") (small-fish "purple")))
  (define pack-four-fish (box-em pack-two-fish-1 pack-two-fish-2))
  (define pack-eight-fish (box-em pack-four-fish pack-four-fish))
  (define pack-sixteen-fish (box-em pack-eight-fish pack-eight-fish))
  
  (slide/title
   "Packing Boxes"
   (page-para "This isn't good enough")
   (hc-append (* 3 gap-size) pack-two-fish-1 pack-two-fish-2)
   (page-para "because it's still two boxes...")
   'next
   (blank)
   (blank)
   (page-para "But this works!")
   pack-four-fish)
  
  (slide/title
   "Packing Boxes"
   (page-para "And here's 8 fish:")
   pack-eight-fish
   'next
   (blank)
   (blank)
   (page-para "And here's 16 fish!")
   (scale pack-sixteen-fish 0.5 0.5)
   'next
   (blank)
   (colorize (page-para* "But what if we just add 1 fish, instead of doubling the fish?")
             BlueColor)
   (colorize (page-para* "But what if we have 0 fish?")
             BlueColor))

  (define (smaller-fish color)
    (scale (small-fish color) 0.5 0.5))
  
  (slide/title
   "General Strategy for Packing Boxes"
   (page-para "Here's a general strategy:")
   (page-item "For 0 fish, use" (code empty))
   (page-item "If you have a package and a new fish, put them together")
   (page-para "To combine many fish, start with" (code empty) "and add fish one at a time")
   'next
   (code empty)
   'next
   (box-em (smaller-fish "red") (code empty))
   'next
   (box-em (smaller-fish "green") (box-em (smaller-fish "red") (code empty)))
   'next
   (box-em (smaller-fish "green") (box-em (smaller-fish "blue") (box-em (smaller-fish "red") (code empty)))))

  (slide/title
   "General Strategy for a List of Numbers"
   (page-para "To represent the aquarium as a list of numbers, use the same idea:")
   (page-item "For 0 fish, use" (code empty))
   (page-item "If you have a list and a number, put them together with" (code make-bigger-list))
   'next
   (code empty)
   'next
   (blank)
   (code (make-bigger-list 10 empty))
   'next
   (blank)
   (scale/improve-new-text (code (make-bigger-list 5 (make-bigger-list 10 empty))) 0.8)
   'next
   (blank)
   (scale/improve-new-text
    (code (make-bigger-list 7 (make-bigger-list 5 (make-bigger-list 10 empty))))
    0.65))
  
  (define lon-defn
    (code (code:comment "A list-of-num is either")
          (code:comment " - empty")
          (code:comment " - (make-bigger-list num list-of-num)")))
  (define lon-defn-gp
    (add-gp-arrow lon-defn 3/4 25/30 1/3 8/30))
  
  (define lon-tmpl
    (code (define (func-for-lon l)
            (cond
              [(empty? l) ...]
              [(bigger-list? l) 
               ... (bigger-list-first l)
               ... (func-for-lon (bigger-list-rest l)) 
               ...]))))
  (define lon-tmpl-gp
    (add-gp-arrow lon-tmpl 1/3 55/70 1/3 8/70))
  
  (with-steps
   (init tmpl-contract tmpl-cond tmpl-parts defn-rec tmpl-rec)
   (slide/title
    "List of Numbers"
    (vl-append
     line-sep
     (cc-superimpose
      ((vbetween-excl init tmpl-rec) lon-defn)
      ((vafter defn-rec) lon-defn-gp))
     (code (define-struct bigger-list (first rest)))
     (tt " ")
     ((vafter tmpl-contract) (t "Generic template:"))
     ((vafter tmpl-contract)
      (code (code:contract func-for-lon : list-of-num -> ...)))
     (lt-superimpose
      ((vbetween tmpl-contract tmpl-contract)
       (code (define (func-for-lon l)
               ...)))
      ((vbetween tmpl-cond tmpl-cond)
       (code (define (func-for-lon l)
               (cond
                 [(empty? l) ...]
                 [(bigger-list? l) ...]))))
      ((vbetween-excl tmpl-parts tmpl-rec)
       (code (define (func-for-lon l)
               (cond
                 [(empty? l) ...]
                 [(bigger-list? l) 
                  ... (bigger-list-first l)
                  ... (bigger-list-rest l)
                  ...]))))
      ((vbetween tmpl-rec tmpl-rec)
       lon-tmpl-gp)))))
  
  (with-steps
   (contract examples1 examples2 examples3 tmpl body stepper)
   (slide/title
    "Aquarium Weight"
    (vl-append
     line-sep
     (code (code:contract aq-weight : list-of-num -> num)
           (code:comment " Sums the fish weights in l"))
     (lt-superimpose
      ((vbetween-excl contract tmpl)
       (code (define (aq-weight l)
               ...)))
      ((vbetween tmpl tmpl)
       (code (define (aq-weight l)
               (cond
                 [(empty? l) ...]
                 [(bigger-list? l) 
                  ... (bigger-list-first l)
                  ... (aq-weight (bigger-list-rest l))
                  ...]))))
      ((vafter body)
       (vl-append
	(/ gap-size 2)
	(code (define (aq-weight l)
		(cond
                 [(empty? l) 0]
                 [(bigger-list? l) 
                  (+ (bigger-list-first l)
                     (aq-weight (bigger-list-rest l)))])))
	((vafter stepper) (colorize (it "Try examples in the stepper") RedColor)))))
     (tt " ")
     ((vafter examples1)
      (code (aq-weight empty) "should be" 0))
     (blank gap-size)
     ((vafter examples2)
      (code (aq-weight (make-bigger-list 2 empty))
            "should be" 2))
     (blank gap-size)
     ((vafter examples3)
      (scale/improve-new-text
       (code (aq-weight (make-bigger-list 5 (make-bigger-list 2 empty))) 
             "should be" 7)
       0.7)))))
     
  (slide/title
   "Shortcuts"
   (page-para "The name" (code make-bigger-list) "is awfully long")
   (blank)
   (page-para "DrScheme has built-in shorter versions")
   (table 3
          (list (code make-bigger-list) sym:implies (code cons)
                (code bigger-list-first) sym:implies (code first)
                (code bigger-list-rest) sym:implies (code rest)
                (code bigger-list?) sym:implies (code cons?))
          lc-superimpose lc-superimpose
          (* 2 gap-size) gap-size)
   'next
   (blank)
   (page-para (code (first (cons 1 empty))) rightarrow (code 1))
   (page-para (code (rest (cons 1 empty))) rightarrow (code empty))
   (page-para (code (cons? empty)) rightarrow (code false)))
  
  (slide/title
   "Lists using the Shortcuts"
   (code (code:comment "A list-of-num is either")
         (code:comment " - empty")
         (code:comment " - (cons num list-of-num)")
         code:blank
         (code:contract aq-weight : list-of-num -> num)
         (define (aq-weight l)
           (cond
             [(empty? l) 0]
             [(cons? l) (+ (first l)
                           (aq-weight (rest l)))]))
         code:blank
         (aq-weight empty) "should be" 0
         code:blank
         (aq-weight (cons 5 (cons 2 empty))) 
         "should be" 7))
  
  (slide/title
   "Design Recipe for Lists"
   (page-para "Design recipe changes for today:")
   (colorize (page-para* "None") BlueColor)
   'next
   (blank)
   (blank)
   (blank)
   (blank)
   (blank)
   (page-para "Granted, the self-reference was slightly novel...")
   (add-gp-arrow (code (code:comment "A list-of-num is either")
                       (code:comment " - empty")
                       (code:comment " - (cons num list-of-num)"))
                 2/3 25/30 1/3 8/30))
  
  (slide/title
   "Recursion"
   (page-para "A self-reference in a data definition leads to a"
              (dt "recursive") "function" (symbol 190) "one that calls itself")
   (add-gp-arrow (code (define (aq-weight l)
                         (cond
                           [(empty? l) 0]
                           [(cons? l) (+ (first l)
                                         (aq-weight (rest l)))])))
                 2/3 45/50 1/3 8/50)
   'next
   (blank)
   (blank)
   (blank)
   (colorize (page-para "Recursion is rumored to be a difficult topic...")
             BlueColor)
   'next
   (colorize (page-para/r "... but now you know better")
             BlueColor))

  )