#cs
(module lecture14 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
	   "utils/lec14.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss"))

  (define big-bad-defn
    (code
     (code:contract big : list-of-nums -> list-of-nums)
     (define (big l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (cond
            [(> (first l) 5) 
             (cons (first l) (big (rest l)))]
            [else (big (rest l))])]))))

  (define huge-bad-defn
    (code
     (code:contract huge : list-of-nums -> list-of-nums)
     (define (huge l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (cond
            [(> (first l) 10) 
             (cons (first l) (huge (rest l)))]
            [else (huge (rest l))])]))))

  (slide/title
   "Big Fish"
   (page-para "A function that gets the big fish (> 5 lbs):")
   (page-para big-bad-defn)
   (page-para
    (code
     (big empty) "should be" empty
     (big '(7 4 9)) "should be" '(7 9))))
  
  (define big-defn
    (code
     (code:contract big : list-of-nums -> list-of-nums)
     (define (big l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define big-rest (big (rest l)))]
            (cond
              [(> (first l) 5) 
               (cons (first l) big-rest)]
              [else big-rest]))]))))
  
  (slide/title
   "Big Fish"
   (page-para "Better with" (code local) ":")
   (page-para big-defn)
   'next
   (blank)
   (colorize (page-para "Suppose we also need to find huge fish...")
             BlueColor))

  (define huge-defn
    (code
     (code:contract huge : list-of-nums -> list-of-nums)
     (define (huge l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define h-rest (huge (rest l)))]
            (cond
              [(> (first l) 10) 
               (cons (first l) h-rest)]
              [else h-rest]))]))))
  
  (slide/title
   "Huge Fish"
   (page-para "Huge fish (> 10 lbs):")
   (page-para huge-defn)
   'next
   (blank)
   (colorize (page-para "How do you suppose I made this slide?")
             BlueColor)
   'next
   (colorize (page-para/r (bit "Cut and Paste!")) RedColor))
  
  (define bug-icon 
    (scale
     (let ([bm (make-object bitmap% (build-path (collection-path "icons")
                                                "bug09.gif"))])
       (send bm set-loaded-mask bm)
       (bitmap bm))
     2))
  (define improve-icon (bitmap "hammer.png"))
  
  (define (avoid s)
    (colorize
     (text s (current-main-font) (* 2 (current-font-size)) (* pi 1/5))
     RedColor))
  
  (with-steps
   (orig want-improve improve new found-bug bug moral)
   (slide/title
    "The Trouble With Cut and Paste"
    (cc-superimpose
     (colorize
      (vl-append (arrow gap-size (* pi -1/4))
                 (vr-append (t "cut and paste")
                            (arrow gap-size (* pi -1/4))))
      GreenColor)
     (rb-superimpose
      (lt-superimpose
       (inset titleless-page (- (* 2 gap-size)))
       (cc-superimpose
        (scale (lt-superimpose 
                ((vbetween orig improve) big-bad-defn)
                ((vafter new) big-defn))
               0.5 0.5)
        ((vbetween found-bug moral) bug-icon)
        ((vbetween want-improve improve) improve-icon)))
      (cc-superimpose
       (scale (lt-superimpose 
               ((vbetween orig improve) huge-bad-defn)
               ((vafter new) huge-defn))
              0.5 0.5)
       ((vbetween bug moral) bug-icon)
       ((vbetween improve improve) improve-icon)))
     ((vafter moral)
      (avoid "Avoid cut and paste!")))
    (cc-superimpose
     ((vbetween improve improve)
      (page-para "After cut-and-paste, improvement is twice as hard"))
     ((vbetween bug moral)
      (page-para "After cut-and-paste, bugs multiply")))))
  
  (define bigger-defn
    (code
     (code:contract bigger : list-of-nums num -> list-of-nums)
     (define (bigger l n)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r (bigger (rest l) n))]
            (cond
              [(> (first l) n) 
               (cons (first l) r)]
              [else r]))]))))
  
  (slide/title
   "How to Avoid Cut-and-Paste"
   'alts
   (list (list
          (page-para "Start with the original function...")
          (page-para big-defn))
         (list
          (page-para "... and add arguments for parts that should change")
          (page-para bigger-defn)
          'next
          (page-para (code (define (big l) (bigger l 5))))
          (page-para (code (define (huge l) (bigger l 10)))))))
  
  (define smaller-defns
    (code
     (code:contract smaller : list-of-nums num -> list-of-nums)
     (define (smaller l n)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r (smaller (rest l) n))]
            (cond
              [(< (first l) n) 
               (cons (first l) r)]
              [else r]))]))
     code:blank
     (define (small l) (smaller l 5))))
  
  (slide/title
   "Small Fish"
   (page-para "Now we want the small fish:")
   'next
   'alts~
   (list (list smaller-defns)
         (list (cc-superimpose
                smaller-defns
                (avoid "Don't cut and paste!")))))
  
  (define sized-defn/no-contract
    (code
     (define (sized l n COMP)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r 
                    (sized (rest l) n COMP))]
            (cond
              [(COMP (first l) n) 
               (cons (first l) r)]
              [else r]))]))))
  
  (define sized-defn/unknown-contract
    (vl-append
     line-sep
     (code (code:contract sized : list-of-nums num ... -> list-of-nums))
     sized-defn/no-contract))

  (define sized-defn
    (vl-append
     line-sep
     (code (code:contract sized : list-of-nums num (num num -> bool) 
                          -> list-of-nums))
     sized-defn/no-contract))
  
  (slide/title
   "Sized Fish"
   (page-para sized-defn/unknown-contract)
   (page-para (code (define (bigger l n) (sized l n >))
                    (define (smaller l n) (sized l n <))))
   'next
   (colorize 
    (page-para* "Does this work? What is the contract for" (code sized) "?")
    BlueColor))
  
  (slide/title
   "Functions as Values"
   (page-para "The definition")
   (code (define (bigger l n) (sized l n >)))
   (page-para "works because" (it "functions are values"))
   'next
   (blank)
   (page-item (code 10) "is a" (code num))
   (page-item (code false) "is a" (code bool))
   'next
   (page-item (code <) "is a" (code (num num -> bool)))
   'next
   (blank)
   (page-para "So the contract for" (code sized) "is")
   (code (code:contract list-of-nums num (num num -> bool) 
                        -> list-of-nums)))
  
  (slide/title
   "Sized Fish"
   (page-para sized-defn)
   'alts
   (list (list (page-para
                (code (define (tiny l) (sized l 2 <))
                      (define (medium l) (sized l 5 =)))))
         (list (blank)
               (colorize
                (page-para* "How about all fish between 3 and 7 lbs?")
                BlueColor))))
  
  (slide/title
   "Mediumish Fish"
   (code (code:contract btw-3-and-7 : num num -> bool)
         (define (btw-3-and-7 a ignored-zero)
           (and (>= a 3)
                (<= a 7)))
         code:blank
         (define (mediumish l) (sized l 0 btw-3-and-7)))
   'next
   (blank)
   (page-item "Programmer-defined functions are values, too")
   (page-item "Note that the contract of" (code btw-3-and-7)
              "matches the kind expected by" (code sized))
   'next
   (blank)
   (page-para "But the ignored" (code 0) "suggests a simplification of" (code sized)
               "..."))
  
  (slide/title
   "A Generic Number Filter"
   (page-para filter-nums-defn)
   'next
   (blank)
   (page-para
    (code (define (btw-3&7 n) (and (>= n 3) (<= n 7)))
          (define (mediumish l) (filter-nums btw-3&7 l)))))
  
  (slide/title
   "Big and Huge Fish, Again"
   (code (define (more-than-5 n)
           (> n 5))
         (define (big l)
           (filter-nums more-than-5 l))
         code:blank
         (define (more-than-10 n)
           (> n 10))
         (define (huge l)
           (filter-nums more-than-10 l)))
   'next
   (blank)
   (page-para "The" (code more-than-5) "and"
              (code more-than-10) "functions are really only useful"
              "to" (code big) "and" (code huge))
   (page-para "We could make them" (code local) "to clarify..."))
  
  (slide/title
   "Big and Huge Fish, Improved"
   (code (define (big l)
           (local [(define (more-than-5 n)
                     (> n 5))]
             (filter-nums more-than-5 l)))
         code:blank
         (define (huge l)
           (local [(define (more-than-10 n)
                     (> n 10))]
             (filter-nums more-than-10 l))))
   'next
   (blank)
   (scale/improve-new-text
    (colorize (bt "Cut and paste alert!") RedColor)
    1.5 1.5)
   (colorize (page-para* "You don't think I typed that twice, do you?")
             BlueColor))
  
  (slide/title
   "Big and Huge Fish, Generalized"
   (code (define (bigger-than l m)
           (local [(define (more-than-m n)
                     (> n m))]
             (filter-nums more-than-m l)))
         code:blank
         (define (big l) (bigger-than l 5))
         (define (huge l) (bigger-than l 10))))

  (define-syntax (scode stx)
    (syntax-case stx ()
      [(_ . c) #'(scale/improve-new-text
                  (code . c)
                  0.8 0.8)]))
  
  (eval-steps
   "Big Example"
   (scode ...
          (define (bigger-than l m)
            (local [(define (more-than-m n)
                      (> n m))]
              (filter-nums more-than-m l)))
          (define (big l) (bigger-than l 5)) ...
          (big '(7 4 9))
          (huge '(7 4 9)))
   #f
   (scode ...
          (define (bigger-than l m)
            (local [(define (more-than-m n)
                      (> n m))]
              (filter-nums more-than-m l)))
          ...
          (bigger-than '(7 4 9) 5)
          (huge '(7 4 9)))
   #f
   (scode ...
          (local [(define (more-than-m n)
                    (> n 5))]
            (filter-nums more-than-m '(7 4 9)))
          (huge '(7 4 9)))
   #f
   (scode ...
          (define (more-than-m42 n)
            (> n 5))
          (filter-nums more-than-m42 '(7 4 9))
          (huge '(7 4 9)))
   (page-para "after many steps")
   (scode ...
          (define (more-than-m42 n)
            (> n 5))
          '(7 9)
          (huge '(7 4 9)))
   #f
   (scode ...
          (define (bigger-than l m)
            (local [(define (more-than-m n)
                      (> n m))]
              (filter-nums more-than-m l)))
          ...
          (define (more-than-m42 n)
            (> n 5))
          '(7 9)
          (bigger-than '(7 4 9) 10))
   #f
   (scode ...
          (define (more-than-m42 n)
            (> n 5))
          '(7 9)
          (local [(define (more-than-m n)
                    (> n 10))]
            (filter-nums more-than-m '(7 4 9))))
   (page-para "Etc.")
   (scode ...
          (define (more-than-m42 n)
            (> n 5))
          '(7 9)
          (define (more-than-m79 n)
            (> n 10))
          (filter-nums more-than-m79 '(7 4 9))))
  
  (slide/title/center
   "Abstraction"
   (page-item "Avoiding cut and paste is" (dt "abstraction"))
   (page-item "No real programming task succeeds without it")
   'next
   (blank)
   (colorize (page-para* "Yes, you will lose points on HW for cut-and-paste code")
             BlueColor))
  
  )
