#cs
(module lecture9 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
           (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (define outline
    (make-outline
     'sort "Sorting a List" #f
     'multiple "Multiple Complex Inputs" #f
     'nat "Natural Numbers" #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'sort)
  (slide/title
   "Sorting Lists"
   (problem "Implement" (code sort-list) ", which takes a list of numbers"
            "and returns a sorted list of the same numbers"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'multiple)
  
  (define append-examples
    (code (append-lists (list 1 3 5) (list 0 4 6)) 
          "should be" (list 1 3 5 0 4 6)))

  (define parallel-examples
    (code (parallel-sum (list 1 3 5) (list 0 4 6)) 
          "should be" (list 1 7 11)))
  
  (define merge-examples
    (code (merge-lists (list 1 3 5) (list 0 4 6)) 
          "should be" (list 0 1 3 4 5 6)))

  (define (mk-ex name examples)
    (let ([c-name (colorize (tt (symbol->string name)) comment-color)]
          [ex-name (colorize (tt (symbol->string name)) id-color)])
      (list
       (scale/improve-new-text 
        (code (code:contract #,c-name : list-of-num list-of-num -> list-of-num))
        0.8)
       (vl-append
        line-sep
        (code (#,ex-name empty empty) "should be" empty)
        (code code:blank)
        examples))))
  
  (slide/title
   "Multiple Complex Arguments"
   (problem "Implement" (code append-lists) ", which takes two lists"
	    "of numbers and returns a list with all of the"
            "numbers from the first list followed by all of the numbers from the second list")
   (problem "Implement" (code parallel-sum) ", which takes two lists"
	    "of numbers (of the same length) and returns a list of sums")
   (problem "Implement" (code merge-lists) ", which takes two" (it "sorted") "lists"
	    "of numbers and returns a sorted list with all of the numbers")
   (blank)
   'alts
   (list (mk-ex 'append-lists append-examples)
         (mk-ex 'parallel-sum parallel-examples)
         (mk-ex 'merge-lists merge-examples)
         (list
          (scale/improve-new-text 
           (code (code:contract func : list-of-num list-of-num -> list-of-num))
           0.8)
          (blank)
          (colorize
           (page-para* "What template do we use for a function for" (it "two") "lists?")
           BlueColor))))

  
  (define complex-title "Multiple Complex Arguments")
  
  (slide/title
   complex-title
   (page-item "Sometimes a complex argument is \"along for the ride\", so use the"
              "template for the other argument")
   (blank)
   append-examples
   (blank)
   (code (define (append-lists al bl)
           (cond
             [(empty? al) ...]
             [(cons? al) 
              ... (first al) 
              ... (append-lists (rest al) bl) ...]))))
  
  (slide/title
   complex-title
   (page-item "Sometimes the arguments are exactly the same shape, so use"
              "essentially the one-argument template")
   (blank)
   parallel-examples
   (blank)
   (scale/improve-new-text 
    (code (define (parallel-sum al bl)
            (cond
              [(empty? al) ...]
              [(cons? al) 
               ... (first al) ... (first bl) 
               ... (parallel-sum (rest al) (rest bl)) ...])))
    0.8))
  
  (slide/title
   complex-title
   (page-item "Sometimes you have to consider all possible combinations, so"
              "use a template that considers all combinations")
   (blank)
   merge-examples
   (blank)
   (scale/improve-new-text 
    (code (define (merge-lists al bl)
            (cond
              [(and (empty? al) (empty? bl)) ...]
              [(and (empty? al) (cons? bl))
               ... (first bl) ... (merge-lists al (rest bl)) ...]
              [(and (cons? al) (empty? bl))
               ... (first al) ... (merge-lists (rest al) bl) ...]
              [(and (cons? al) (cons? bl))
               ... (first al) ... (first bl) 
               ... (merge-lists (rest al) bl)
               ... (merge-lists al (rest bl))
               ... (merge-lists (rest al) (rest bl)) ...])))
    0.75))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'nat)
  
  (slide/title
   "Numbers to Generate Lists"
   (problem "Implement" (code create-list) ", which takes a non-negative integer"
            (it "n") "and produces a list of numbers from" (it "n") "to 0, inclusive")
   (blank)
   (code (code:contract create-list : num -> list-of-num)
         code:blank
         (create-list 3) "should be" (list 3 2 1 0)
         code:blank
         (create-list 0) "should be" (list 0))
   'next
   (blank)
   (blank)
   (colorize
    (page-para "The template for" (code num) "isn't much help:")
    BlueColor)
   (code (define (func-for-num n)
           ...))
   'next
   (colorize
    (page-para "But" (code create-list) "actually takes a" (dt "natural number"))
    BlueColor))

  (define nat-defn
    (code (code:comment "A nat is either")
          (code:comment " - 0")
          (code:comment " - (add1 nat)")))

  (slide/title
   "Natural Numbers"
   nat-defn
   (colorize (page-para "Examples:") BlueColor)
   (code 0)
   (code (add1 0))
   (code (add1 (add1 (add1 0))))
   'next
   (blank)
   (blank)
   (page-para "These examples have shortcuts")
   (page-para* (code 0) "," (code 1) ", and" (code 3))
   (page-para "but the long forms correspond to the template"))
  
  (slide/title
   "Template for Natural Numbers"
   nat-defn
   (blank)
   (code (define (func-for-nat n)
           (cond
             [(zero? n) ...]
             [else  ... (func-for-nat (sub1 n)) ...])))
   'next
   (blank)
   (code (define (create-list n)
           (cond
             [(zero? n) (list 0)]
             [else  (cons n (create-list (sub1 n)))]))))
  
  (slide/title
   "Generating the List the Other Way"
   (problem "Implement" (code create-up-list) ", which takes a non-negative integer"
            (it "n") "and produces a list of numbers from 0 to" (it "n") "inclusive")
   (blank)
   (code (code:contract create-up-list : num -> list-of-num)
         code:blank
         (create-list 3) "should be" (list 0 1 2 3)
         code:blank
         (create-list 0) "should be" (list 0))
   'next
   (code (define (create-up-list n)
           (cond
             [(zero? n) (list 0)]
             [else  
              ... n 
              ... (create-up-list (sub1 n)) ...]))
         (code:comment "uh oh... can't cons onto recur result")))
  
  (slide/title
   "Using Subtraction to Count Up"
   (code (define (create-up-list n)
           (create-up-to-n-list n n))
         code:blank
         (code:comment "Creates a list with d elements before n")
         (define (create-up-to-n-list d n)
           (cond
             [(zero? d) (list n)]
             [else  
              (cons (- n d)
                    (create-up-to-m-list (sub1 d) n))])))
   'next
   (blank)
   (page-para "... or replace" (code d) "with" (code m = (+ d n)))
   (page-para "As" (code d) "goes down," (code m) "goes up..."))
  
  (slide/title
   "Counting Up Directly"
   (code (define (create-up-list n)
           (create-m-to-n-list 0 n))
         code:blank
         (code:comment "Creates a list from m to n")
         (define (create-m-to-n-list m n)
           (cond
             [(= m n) (list n)]
             [else  
              (cons m
                    (create-m-to-n-list (add1 m) n))])))
   'next
   (blank)
   (colorize (it "Use the stepper to see how it works") RedColor)
   'next
   (blank)
   (page-para "Similar ideas work for counting by fives, counting down to 20, etc."))
  
  )


