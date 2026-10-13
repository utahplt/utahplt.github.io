#cs
(module lecture19 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
	   "utils/utils.ss"
           "utils/code.ss"
           "utils/eval-step.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  
  (slide/title
   "Designing Generative Recusion"
   (page-para*/c "When you discover that the design recipe isn't working,"
                 (colorize (bt "stop writing code") RedColor))
   'next
   (blank)
   (blank)
   (page-para "Instead, figure out the" (it "algorithm"))
   (page-item "What is the trivial case?")
   (page-item "What are the smaller sub-problems, and how are their solutions combined?")
   'next
   (blank)
   (colorize
    (page-para* "Generating sub-problems or combining the answers may require additional functions")
    BlueColor))
  
  (slide/title
   "Generating Sub-Problems"
   (page-para "The key to a sub-problem is that it looks like the original problem (only smaller)")
   (page-para (colorize (t "Example:") BlueColor)
              "In" (code odd-items) ", the sub-problem is a smaller list from which we want the odd items")
   (page-para (colorize (t "Homework:") BlueColor)
              "In" (code colors->list) ", the sub-problem should be a smaller list from which to extract rows")
   'next
   (blank)
   (page-para (colorize (t "Guideline:") GreenColor)
              "When the result is a list, try to generate the"
              "first item in the list, then create a sub-problem for the rest of the list"))

  (define c->c-ex
    (code (colors->columns (list color1 color2 color3
                                 color4 color5 color6)
                           3)
          "should be" (list (list color1 color4)
                            (list color2 color5)
                            (list color3 color6))))

  (slide/title/center
   "New Example"
   (page-para "Suppose that instead of rows, we want to convert an image into a list of columns")
   c->c-ex
   (blank)
   (colorize (page-para* "Structural recursion doesn't work well") BlueColor))

  (define c->c-questions
    (vc-append
     gap-size
     (page-para "The result is a list of columns:")
     (page-item "Can we get the first column?")
     (page-item "Can we create a list with only the other columns?")))

  (define sep-line (colorize (hline (* 3/4 client-w) 1) GreenColor))
  
  (slide/title
   "Designing the Column Converter"
   c->c-ex
   'alts
   (list (list
          c->c-questions)
         (list
          sep-line
          (vl-append
           line-sep
           (code (colors->columns (list color1 color2 color3
                                        color4 color5 color6)
                                  3))
           sym:rightarrow
           (code
            (cons (list color1 color4)
                  (colors->columns (list color2 color3
                                         color5 color6)
                                   2)))))
         (list
          sep-line
          (code (code:contract extract-first-column :
                               code:blank list-of-color num -> list-of-color)
                code:blank
                (code:contract drop-first-column :
                               code:blank list-of-color num -> list-of-color)))))
          
  (slide/title
   "Implementing the Column Converter"
   (code (define (colors->columns l n)
           (cond
             [(empty? l) empty]
             [else 
              (local [(define c1 
                        (extract-first-column l n))
                      (define rl
                        (drop-first-column l n))]
                (cons c1
                      (colors->columns rl (sub1 n))))])))
   (blank)
   (blank)
   (colorize
    (page-para/r "With two pending wishes...")
    RedColor))
  
  (define efc-questions
    (vc-append
     gap-size
     (page-item "Can we get the first item in the column?")
     (page-item "Can we create a list whose first column is the rest of the column?")))
   
  (slide/title
   "Designing Extract"
   (page-para "Now to satisfy our wish for" (code extract-first-column) "...")
   (scale/improve-new-text
    (code (extract-first-column (list color1 color2 color3
                                      color4 color5 color6)
                                3)
          "should be" (list color1 color4))
    0.9)
   'next
   'alts
   (list (list
          (blank)
          (colorize
           (page-para* "Again, structural recursion doesn't work well")
           BlueColor)
          (blank)
          efc-questions)
         (list
          sep-line
          (scale/improve-new-text
           (vl-append
            line-sep
            (code (extract-first-column (list color1 color2 color3
                                              color4 color5 color6)
                                        3))
            sym:rightarrow
            (code (cons color1
                        (extract-first-column 
                         (list color4 color5 color6)
                         3))))
           0.9)
          'next
          (blank)
          (code (code:contract skip-n : list-of-X nat -> list-of-X)))))
  
  (slide/title
   "Implementing Extract"
   (code (define (extract-first-column l n)
           (cond
             [(empty? l) empty]
             [else 
              (cons 
               (first l)
               (extract-first-column (skip-n l n) n))])))
   (blank)
   (blank)
   (page-para "Implementing" (code skip-n) "is an exercise in structural recursion on" (code nat)))
  
  (define drop-ex
    (scale/improve-new-text
     (code (drop-first-column (list color1 color2 color3
                                    color4 color5 color6)
                              3)
           "should be" (list color2 color3
                             color5 color6))
     0.9))
   
  (define drop-main-question
    (page-item "Can we create a list where dropping the first column is the rest of the answer?"))
  (define drop-questions
    (vc-append
     gap-size
     (colorize
      (page-para* "Yet again, structural recursion doesn't work well")
      BlueColor)
     (page-item "Can we get the first item in the result?")
     drop-main-question))
  
  (slide/title
   "Designing Drop"
   (page-para "Finally, to satisfy our wish for" (code drop-first-column) "...")
   drop-ex
   'next
   (blank)
   'alts
   (list (list drop-questions)
         (list sep-line
               (scale/improve-new-text
                (vl-append
                 line-sep
                 (code (drop-first-column (list color1 color2 color3
                                                color4 color5 color6)
                                          3))
                 sym:rightarrow
                 (code (cons color2
                             (drop-first-column _??? 3))))
                0.9))
         (list
          sep-line
          drop-main-question
          (colorize
           (page-para* "No" sym:emdash "getting just the first item doesn't make a similar sub-problem")
           RedColor))
         (list
          sep-line
          (page-para "Need to grab an entire row, then skip the row to recur")
          (scale/improve-new-text
           (vl-append
            line-sep
            (code (drop-first-column (list color1 color2 color3
                                           color4 color5 color6)
                                     3))
            sym:rightarrow
            (code (append (list color2 color3)
                          (drop-first-column (list color4 color5 color6) 3))))
           0.8))))

  (slide/title
   "Implementing Drop"
   (code (define (drop-first-column l n)
           (cond
             [(empty? l) empty]
             [else
              (append 
	       (first-n (rest l) (sub1 n))
	       (drop-first-column (skip-n l n)))]))
         code:blank
         code:blank
         (code:contract first-n : list-of-X nat -> list-of-X)
         (code:contract snip-n : list-of-X nat -> list-of-X))
   (blank)
   (page-para "The leftover wishes are strightforward"))
  
  (define rr-ex
    (code (replace-range '(a b c d e) 1 3 'x)
          "should be"
          '(a x x x e)))
    
  (slide/title
   "Another Example"
   (with-font 
    'roman
    (lambda ()
      (problem "Implement" (code replace-range)
               ", which takes a list, two numbers" (it "start") "and" (it "end")
               ", and a value" (it "v")
               "; the result is a list like the given one, except that" (it "v")
               "replaces the elements in positions" (it "start") "to" (it "end")
               "inclusive")))
   'next
   (blank)
   (vl-append
    line-sep
    (code (code:contract replace-range :
                         code:blank list-of-X num num X -> list-of-X)
          code:blank)
    rr-ex))
  
  (with-steps
   (basic another more)
   (slide/title
    "Designing Replacement"
    rr-ex
    sep-line
    (lt-superimpose
     ((vbefore more)
      (vl-append
       line-sep
       (code (replace-range '(a b c d e) 1 3 'x))
       sym:rightarrow
       (code (cons 'a
                   (replace-range '(b c d e) 0 2 'x)))
       ((vafter another) sym:rightarrow)
       ((vafter another)
        (code (cons 'a
                    (cons 'x
                          (replace-range '(c d e) -1 1 'x)))))))
     ((vafter more)
      (vl-append
       line-sep
       (hbl-append gap-size sym:rightarrow sym:rightarrow)
       (code (cons 'a
                   (cons 'x
                         ... 
                         (replace-range '(e) -3 -1 'x))))
       sym:rightarrow
       (code (cons 'a 
                   ... ...
                   (cons 'e 
                         (replace-range empty -4 -2 'x)))))))))
  
  (slide/title
   "Implementing Replacement"
   (code (define (replace-range l s e v)
           (cond
             [(empty? l) empty]
             [else (cons (cond
                           [(and (< s 1) (> e -1)) v]
                           [else (first l)])
                         (replace-range (rest l)
                                        (sub1 s)
                                        (sub1 e)
                                        v))]))))
 
  (slide/title/center
   "Designing Generative Recursion"
   (page-para* "Finding the recursive sub-problem is the key")
   (blank)
   (page-item "Think first, write code second")
   (page-item "Writing down example steps can help"))
  
   )
