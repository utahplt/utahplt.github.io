#cs
(module lecture18 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  
  (slide/title/center
   "Max of a List"
   (problem "Implement the function" (code max-item)
            "which returns the biggest number in a list of numbers"))
  
  (slide/title/center
   "Data and Contract"
   (page-para (bt "Data:") (code list-of-num) ", obviously")
   (blank)
   (page-para (bt "Contract:"))
   (code (code:contract max-item : list-of-num -> num)))
    
  (slide/title/center
   "Examples"
   (code (max-item '(2 7 5)) "should be" 7)
   'next
   (code (max-item empty) "should be" ...)
   'next
   (blank)
   (colorize
    (page-para* "Problem:" (code max-item) "makes no sense on an empty list")
    RedColor))

  (define nelon-defn
    (code (code:comment "A nonempty-list-of-num is either")
          (code:comment " - (cons num empty)")
          (code:comment " - (cons num nonempty-list-of-num)")))

  (slide/title/center
   "Data and Contract, Again"
   (page-para (bt "Data:") (code nonempty-list-of-num))
   nelon-defn
   'next
   (blank)
   (page-para (bt "Contract:"))
   (code (code:contract max-item : nonempty-list-of-num -> num)))
  
  
  (slide/title/center
   "Examples, Again"
   (code (max-item '(2 7 5)) "should be" 7)
   (code (max-item '(2)) "should be" 2))
  
  (slide/title
   "Implementation"
   (page-para "No existing functions on non-empty lists, so start with the template")
   (blank)
   nelon-defn
   'next
   (blank)
   (code (define (max-item nel)
           (cond
             [(empty? (rest nel)) ... (first nel) ...]
             [else 
              ... (first nel)
              ... (max-item (rest nel)) ...]))))
  
  (slide/title/center
   "Implementation Complete"
   (code (define (max-item nel)
           (cond
             [(empty? (rest nel)) (first nel)]
             [else 
              (cond
                [(> (first nel) (max-item (rest nel)))
                 (first nel)]
                [else
                 (max-item (rest nel))])]))))

  (slide/title
   "Test"
   (page-para
    (code (max-item '(2)) "should be" 2))
   (colorize (page-para/r "works fine") BlueColor)
   'next
   (blank)
   (page-para
    (code (max-item '(1 2 3 4 5 6 7 8 9 10))
          "should be" 10))
   (colorize (page-para/r "works fine") BlueColor)
   'next
   (blank)
   (page-para
    (code (max-item '(1 2 3 4 5 6 7 8 9 10
                        11 12 13 14 15 16 17 18 19 20
                        21 22 23 24 25 26 27 28 29 30))
          "should be" 30))
   (colorize (page-para/r "answer never appears!") RedColor))
  
  (slide/title/center
   "The Speed of max-item"
   (page-para "Somewhere around 20 items, the" (code max-item)
              "function starts to take way too long")
   'next
   (page-para "Even if you buy a computer that's 10 times faster, the"
              "problem shows up with about 23 items...")
   'next
   (blank)
   (colorize
    (page-para* "How can we understand how long a program takes to run?")
    BlueColor))
  
  (slide/title
   "Counting Steps"
   (page-para "How long does")
   (code (+ 1 (* 6 7)))
   (page-para "take to execute?")
   'next
   (blank)
   (blank)
   (page-para "Computer speeds differ in \"real time\", but we can count steps:")
   (page-para* (code (+ 1 (* 6 7)))
               (blank) sym:rightarrow (blank)
               (code (+ 1 42))
               (blank) sym:rightarrow (blank)
               (code 43))
   (page-para "So, evaluation takes 2 steps"))
  
  (define (simple-steps . s)
    (vl-append
     line-sep
     (car s)
     (let loop ([s (cdr s)])
       (if (null? (cdr s))
           (hbl-append gap-size sym:rightarrow (car s))
           (vl-append
            line-sep
            (loop (list (car s)))
            (loop (cdr s)))))))
  
  (slide/title
   "Steps for max-item and 1 Element"
   (page-para "How long does this expression take?")
   (code (max-item '(2)))
   'next
   (blank)
   (scale/improve-new-text
    (simple-steps (code (max-item '(2)))
                  (code (cond [(empty? (rest '(2))) (first '(2))] ...))
                  (code (cond [(empty? empty) (first '(2))] ...))
                  (code (cond [true (first '(2))] ...))
                  (code (first '(2)))
                 (code 2))
    0.9)
   (blank)
   (page-para "5 steps" sym:emdash "and any list with one item will take five steps"))
  
  (define (skip4) (hbl-append gap-size 
                              (code ...) sym:rightarrow
                              (code ...) sym:rightarrow
                              (code ...) sym:rightarrow
                              (code ...)))

  (slide/title
   "Steps for max-item and 2 Elements"
   (page-para "How long does this expression take?")
   (code (max-item '(2 1)))
   'next
   (blank)
   (scale/improve-new-text
    (simple-steps (code (max-item '(2 1)))
                  (code (cond [(empty? (rest '(2 1))) (first '(2 1))] [else ...]))
                  (code (cond [(empty? '(1)) (first '(2 1))] [else ...]))
                  (code (cond [false (first '(2 1))] [else ...]))
                  (code (cond [else (cond [(> (first '(2 1)) ...) ...] [else ...])]))
                  (code (cond [(> (first '(2 1)) (max-item (rest '(2 1)))) ...] [else ...]))
                  (code (cond [(> 2 (max-item (rest '(2 1)))) ...] [else ...]))
                  (code (cond [(> 2 (max-item '(1))) ...] [else ...]))
                  (skip4)
                  (code (cond [(> 2 1) (first '(2 1))] [else ...]))
                  (code (first '(2 1)))
                  (code 2))
    0.6)
   'next
   (blank)
   (page-para "14 steps" sym:emdash "where 5 came from the recursive call")
   'next
   (colorize
    (page-para* "Are all 2-element lists the same?")
    BlueColor))

  (slide/title
   "Steps for max-item and 2 Elements"
   (code (max-item '(1 2)))
   'next
   (blank)
   (scale/improve-new-text
    (simple-steps (code (max-item '(1 2)))
                  (code (cond [(empty? (rest '(1 2))) (first '(1 2))] [else ...]))
                  (code (cond [(empty? '(2)) (first '(1 2))] [else ...]))
                  (code (cond [false (first '(1 2))] [else ...]))
                  (code (cond [else (cond [(> (first '(1 2)) ...) ...] [else ...])]))
                  (code (cond [(> (first '(1 2)) (max-item (rest '(1 2)))) ...] [else ...]))
                  (code (cond [(> 1 (max-item (rest '(1 2)))) ...] [else ...]))
                  (code (cond [(> 1 (max-item '(2))) ...] [else ...]))
                  (skip4)
                  (code (cond [(> 1 2) ...] [else ...]))
                  (code (cond [else (max-item (rest '(1 2)))]))
                  (code (max-item (rest '(1 2))))
                  (code (max-item '(2)))
                  (skip4)
                  (code 2))
    0.6)
   'next
   (blank)
   (page-para "20 steps" sym:emdash "where 10 came from" (it "two") "recursive calls"))

  (define super-scr
    (opt-lambda (s [extra-style null])
      (text s `(,@extra-style superscript . ,main-font) (current-font-size))))
  (define (sub-scr s)
    (text s `(subscript . ,main-font) (current-font-size)))

  (slide/title
   "Steps for max-item and N Elements"
   (page-para "In the worst case, the step count" (bt "T")
              "for an" (hbl-append (it "n") (t "-element"))
              "list passed to" (code max-item) "is")
   (hbl-append (bt "T") (t "(") (it "n") (t ") = 10 + 2") 
               (bt "T") (t "(") (it "n") (t "-1)"))
   'next
   (blank)
   (vl-append
    line-sep
    (hbl-append (bt "T") (t "(1) = 5"))
    (hbl-append (bt "T") (t "(2) = 10 + 2") (bt "T") (t "(1) = 20"))
    (hbl-append (bt "T") (t "(3) = 10 + 2") (bt "T") (t "(2) = 50"))
    (hbl-append (bt "T") (t "(4) = 10 + 2") (bt "T") (t "(3) = 110"))
    (hbl-append (bt "T") (t "(5) = 10 + 2") (bt "T") (t "(4) = 230"))
    (t "..."))
   'next
   (page-item "In general, " (hbl-append (bt "T") (t "(") (it "n") (t ") > 2") 
                                         (super-scr "n")))
   (page-item "Note that" (hbl-append (t "2") (super-scr "30"))
              "is 1,073,741,824" sym:emdash "which is why our last test never produced a result"))
  
  (slide/title
   "Repairing max-item"
   (page-para "In the case of" (code max-item) ", the problem is easily fixed with" (code local))
   (code (define (max-item nel)
           (cond
             [(empty? (rest nel)) (first nel)]
             [else 
              (local [(define r (max-item (rest nel)))]
                (cond
                  [(> (first nel) r) (first nel)]
                  [else r]))])))
   (blank)
   (page-para "With this definition, there's always one recursive call")
   (page-para (code (max-item '(1 2))) "takes 17 steps"))
  
  (slide/title
   "Steps for new max-item and N Elements"
   (page-para "In the worst case, now, the step count" (bt "T")
              "for an" (hbl-append (it "n") (t "-element"))
              "list passed to" (code max-item) "is")
   (hbl-append (bt "T") (t "(") (it "n") (t ") = 12 + ") 
               (bt "T") (t "(") (it "n") (t "-1)"))
   'next
   (blank)
   (vl-append
    line-sep
    (hbl-append (bt "T") (t "(1) = 5"))
    (hbl-append (bt "T") (t "(2) = 12 + ") (bt "T") (t "(1) = 17"))
    (hbl-append (bt "T") (t "(3) = 12 + ") (bt "T") (t "(2) = 29"))
    (hbl-append (bt "T") (t "(4) = 12 + ") (bt "T") (t "(3) = 41"))
    (hbl-append (bt "T") (t "(5) = 12 + ") (bt "T") (t "(4) = 53"))
    (t "..."))
   'next
   (page-item "In general, " (hbl-append (bt "T") (t "(") (it "n") (t ") = 5 + 12(") (it "n") (t "-1)")))
   (page-item "So our last test takes only 343 steps"))

  (slide/title/center
   "Using Local to Reduce Complexity"
   (page-item "Before, we used" (code local) "to either make the code nicer"
	      "or to support abstraction")
   (page-item "Now we're using" (code local) "to avoid redundant calculations,"
	      "which avoids evaluation complexity")
   (page-para "Fortunately, these reasons reinforce each other")
   (blank)
   (colorize
    (page-para* "Where a value is definitely computed and possibly computed multiple times,"
		"always give it a name and compute it once")
    BlueColor))


  (define sort-defn
   (scale/improve-new-text
    (code 
     (code:contract sort-list : list-of-num -> list-of-num)
     (define (sort-list l)
       (cond
         [(empty? l) empty]
         [(cons? l) (insert (first l) (sort-list (rest l)))])))
    0.8))

  (define insert-defn
    (scale/improve-new-text
     (code 
      (code:contract insert : num list-of-num -> list-of-num)
      (define (insert n l)
        (cond
          [(empty? l) (list n)]
          [(cons? l)
           (cond
             [(< n (first l)) (cons n l)]
             [else (cons (first l) (insert n (rest l)))])])))
     0.8))

  (slide/title
   "Sorting"
   (page-para "We once wrote a" (code sort-list) "function:")
   sort-defn
   'next
   (blank)
   (page-para "How long does it take to sort a list of"
              (it "n") "numbers?")
   'next
   (blank)
   (page-para "We have only one recursive call to" (code sort-list)
              ", so it doesn't have the same problem as before..."))
  
  (slide/title
   "Insertion Sort"
   (page-para "... but what about" (code insert) "?")
   (vl-append
    gap-size
    sort-defn
    insert-defn)
   (page-para "On each iteration of" (code sort-list)
              ", there's a call to" (code sort-list) "and a call to" (code insert)))
  
  (define insert-time
    (hbl-append (it "k") (sub-scr "1") (t " + ") (it "k") (sub-scr "2") (it "n")))

  (slide/title
   "Insert Time"
   (page-para (code insert) "itself is like the repaired" (code max-item) ":")
   insert-defn
   (page-para "In the worst case," (code insert) "into a list of size" (it "n")
              "takes" insert-time)
   (page-para "The variables" 
              (hbl-append (it "k") (sub-scr "1")) "and" (hbl-append (it "k") (sub-scr "2"))
              "stand for some constant"))
  
  (define insert-sort-time
    (vl-append
     line-sep
     (hbl-append (bt "T") (t "(0) = ") (it "k") (sub-scr "3"))
     (hbl-append (bt "T") (t "(") (it "n") (t ") = ")
                 (it "k") (sub-scr "4") (t " + ")
                 (bt "T") (t "(") (it "n") (t "-1)")
                 (t " + ")
                 (hbl-append (it "k") (sub-scr "1") (t " + ") (it "k") (sub-scr "2") (it "n")))))
  
  (slide/title
   "Insertion Sort Time"
   (page-para "Given that the time for" (code insert) "is" insert-time "...")
   (blank)
   sort-defn
   (blank)
   (page-para "The time for" (code sort-list) "is defined by")
   insert-sort-time)

  (slide/title
   "Insertion Sort Time"
   insert-sort-time
   (blank)
   (page-para "Even if each" (it "k") "were only 1:")
   (vl-append
    line-sep
    (hbl-append (bt "T") (t "(0) = 1"))
    (hbl-append (bt "T") (t "(1) = 4"))
    (hbl-append (bt "T") (t "(2) = 8"))
    (hbl-append (bt "T") (t "(2) = 13"))
    (hbl-append (bt "T") (t "(3) = 19"))
    (t "..."))
   (blank)
   (page-item "In the long run, " (hbl-append (bt "T") (t "(") (it "n") (t ")"))
              "is a lot like" (hbl-append (it "n") (super-scr "2")))
   (page-item "This is a lot better than" (hbl-append (t "2") (super-scr "n" '(italic)))
              sym:emdash "but sorting a list of 10,000 items takes more than"
              "100,000,000 steps"))
  
  (slide/title/center
   "Sorting Algorithms"
   (page-item "The" (code list-of-num) "template leads to the" (dt "insertion sort") "algorithm")
   (page-subitem "It's not practical for large lists")
   (page-item "Algorithms such as" (dt "quick sort") "and" (dt "merge sort") "are faster"))
  
  (slide/title/tall
   "Merge Sort"
   (scale/improve-new-text
    (code (define (merge-sort l)
            (cond
              [(or (empty? l) (empty? (rest l))) l]
              [else 
               (local [(define a-half (even-items l))
                       (define b-half (odd-items l))]
                 (merge-lists
                  (merge-sort a-half)
                  (merge-sort b-half)))])))
    0.8)
   (blank)
   (page-item (code even-items) "and" (code odd-items) "each take"
              (hbl-append (it "k") (sub-scr "5") (t " + ") (it "k") (sub-scr "6")  (it "n")) "steps")
   (page-item (code merge-lists) "takes"
              (hbl-append (it "k") (sub-scr "7") (t " + ") (it "k") (sub-scr "8")  (it "n")) "steps")
   (page-item "So, for" (code merge-sort) ":")
   (vl-append
    line-sep
    (hbl-append (bt "T") (t "(0) = ") (it "k") (sub-scr "9"))
    (hbl-append (bt "T") (t "(1) = ") (it "k") (sub-scr "10"))
    (hbl-append (bt "T") (t "(") (it "n") (t ") = ")
                (it "k") (sub-scr "11") (t " + ")
                (t "2") (bt "T") (t "(") (it "n") (t "/2)")
                (t " + ")
                (hbl-append (t "2") (it "k") (sub-scr "5") (t " + ") 
                            (t "2") (it "k") (sub-scr "6") (it "n"))
                (t " + ")
                (hbl-append (it "k") (sub-scr "7") (t " + ") 
                            (it "k") (sub-scr "8") (it "n")))))
  
  (slide/title/tall
   "Merge Sort Time"
   (page-para "Simplify by collapsing constants:")
   (hbl-append (bt "T") (t "(") (it "n") (t ") = ")
               (it "k") (sub-scr "12") (t " + ")
               (t "2") (bt "T") (t "(") (it "n") (t "/2)")
               (t " + ")
               (hbl-append (it "k") (sub-scr "13") (it "n")))
   (page-para "Setting constants to 1:")
   (vl-append line-sep
              (t "...")
              ; (hbl-append (bt "T") (t "(0) = 1"))
              ; (hbl-append (bt "T") (t "(1) = 1"))
              ; (hbl-append (bt "T") (t "(2) = 5"))
              ; (hbl-append (bt "T") (t "(3) = 10"))
              ; (hbl-append (bt "T") (t "(4) = 15"))
              (hbl-append (bt "T") (t "(5) = 21"))
              (hbl-append (bt "T") (t "(6) = 27"))
              (hbl-append (bt "T") (t "(7) = 33"))
              (hbl-append (bt "T") (t "(8) = 39"))
              (hbl-append (bt "T") (t "(9) = 46"))
              (t "..."))
   (page-para "In the long run," (hbl-append (bt "T") (t "(") (it "n") (t ")"))
              "is a lot like" (hbl-append (it "n") (t "log") (sub-scr "2") (it "n")))
   (page-item "Sorting a list of 10,000 items takes something like"
              "100,000 steps (which is 1,000 times faster than insertion sort)"))
  
  (slide/title
   "The Cost of Computation"
   (page-para "The study of execution time is called" (dt "complexity theory"))
   (blank)
   (page-para "Practical points:")
   (page-item/bullet (bt "1.") "Use" (code local) "to avoid redundant computations")
   (page-subitem "Something you can always do to tame evaluation")
   (page-item/bullet (bt "2.") "Algorithms like" (code merge-sort) "are in textbooks")
   (page-subitem "You learn them, not invent them")
   'next
   (blank)
   (colorize
    (page-para* "Other courses teach you more about the second category")
    BlueColor)
   (colorize
    (page-para* "Is there anything else in the first category (things you can do now)?")
    BlueColor)
   'next
   (colorize (page-para/r "soon...") RedColor))
  
  (slide/title
   "Vectors"
   (page-para "The" (bt "Advanced") "language provides" (dt "vectors") ", which is similar to lists:")
   (code
    > (list 1 2 3)
    (list 1 2 3)
    > (vector 1 2 3)
    (vector 1 2 3))
   (blank)
   (page-para "Differences:")
   (page-item "There's nothing like" (code cons) "for vectors")
   (page-item "The" (code vector-ref) "function extracts an element from"
              "anywhere in the vector in constant time"))
  
  (slide/title
   "List-Ref versus Vector-Ref"
   'alts
   (list
    (list
     (code (code:contract list-ref : list-of-X nat -> X)
           (define (list-ref l n)
             (cond
               [(zero? n) (first l)]
               [else (list-ref (rest l) (sub1 n))]))
           code:blank
           (list-ref '(a b c d) 1) "should be" 'b)
     (page-para "In general," (code (list-ref _l _n)) "takes about" (code _n) "steps"))
    (list
     (code (code:contract vector-ref : vector-of-X nat -> X)
           (define (vector-ref l n)
             ...)
           code:blank
           (vector-ref (vector 'a 'b 'c 'd) 1)
           "should be" 'b)
     (page-para "In general," (code (vector-ref _v _n)) "takes 1 step")
     (colorize
      (page-para/r "You can't actually define" (code vector-ref) "yourself")
      BlueColor)
     (blank)
     (blank)
     (page-para "Eventually, we'll use vectors when we need"
                "\"random access\" among arbitrarily many components")
     (page-para "More generally, each kind of data comes with"
                "operations that have a certain cost" sym:emdash
                "a programmer has to pick the right data"))))
   
   
  
  )
