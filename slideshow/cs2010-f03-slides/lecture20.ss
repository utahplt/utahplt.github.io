#cs
(module lecture20 (lib "slideshow.ss" "slideshow")  
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
  
  (define (mk-fish color size open?)
   (standard-fish (* 3 gap-size (sqrt size)) (* 2 gap-size (sqrt size)) 'left color "white" open?))
  
  (define before-fish
    (hc-append gap-size (mk-fish "red" 3 #t) (mk-fish "green" 2 #t) (mk-fish "blue" 3 #t)))
  (define after-fish
    (hc-append gap-size (mk-fish "red" 3 #f) (mk-fish "green" 5 #f) (mk-fish "blue" 8 #f)))
  
  (with-steps
   (pic scm)
   (slide/title
    "The Food Chain"
    (problem "Implement the function"
             (code food-chain)
             "which takes a list of fish and returns"
             "a list of fish where each has eaten all of the fish to the left")
    (blank)
    (lt-superimpose
     ((vonly pic)
      (in-aq (cc-superimpose before-fish (ghost after-fish))))
     ((vonly scm)
      (code (food-chain '(3 2 3)))))
    sym:rightarrow
    (lt-superimpose
     ((vonly pic)
      (in-aq (cc-superimpose after-fish (ghost before-fish))))
     ((vonly scm)
      (code '(3 5 8))))))

  (slide/title
   "Implementing the Food Chain"
   (code (define (food-chain l)
           (cond
             [(empty? l) ...]
             [else 
              ... (first l)
              ... (food-chain (rest l)) ...])))
   'next
   (blank)
   (page-para "Is the result of"
              (code (food-chain '(2 3)))
              "useful for getting the result of"
              (code (food-chain '(3 2 3)))
              "?")
   (blank)
   (vl-append
    (code (food-chain '(3 2 3)))
    (page-para* sym:rightarrow
                (code ... 3 ... (food-chain '(2 3)) ...))
    (page-para* sym:rightarrow
                (code ... 3 ... '(2 5) ...))
    (page-para* sym:rightarrow sym:rightarrow
                (code '(3 5 8)))))

  (define food-chain-defn
    (code
     (define (food-chain l)
       (cond
         [(empty? l) empty]
         [else 
          (cons (first l)
                (feed-fish (food-chain (rest l))
                           (first l)))]))))
         

  (define feed-fish-defn
    (code
     (define (feed-fish l n)
       (cond
         [(empty? l) empty]
         [else (cons (+ n (first l))
                     (feed-fish (rest l) n))]))))

  (slide/title
   "Implementing the Food Chain"
   (page-para "Feed the first fish to the rest, then" (code cons) ":")
   (vl-append
    line-sep
    food-chain-defn
    (code code:blank)
    feed-fish-defn))
  
  (define super-scr
    (opt-lambda (s [extra-style null])
      (text s `(,@extra-style superscript . ,main-font) (current-font-size))))
  (define (sub-scr s)
    (text s `(subscript . ,main-font) (current-font-size)))

  (define chain-T-defn
    (vl-append
     line-sep
     (hbl-append (dt "T") (t "(0) = ") (it "k") (sub-scr "1"))
     (hbl-append (dt "T") (t "(") (it "n") (t ") = ") (it "k") (sub-scr "2") 
                 (t " + ") (dt "T") (t "(") (it "n") (t "-1) + ")
                 (dt "S") (t "(") (it "n") (t "-1)"))))
   
  (slide/title
   "The Cost of the Food Chain"
   (page-para "How long does" (code (feed-fish l)) "take when"
              (code l) "has" (it "n") "fish?")
   'next
   (blank)
   food-chain-defn
   'next
   (blank)
   chain-T-defn
   (page-para/r "where" (hbl-append (dt "S") (t "(") (it "n") (t ")"))
                "is the cost of" (code feed-fish)))
  
  (slide/title
   "The Cost of the Food Chain with feed-fish"
   chain-T-defn
   (blank)
   (blank)
   feed-fish-defn
   (blank)
   (vl-append
    line-sep
    (hbl-append (dt "S") (t "(0) = ") (it "k") (sub-scr "3"))
    (hbl-append (dt "S") (t "(") (it "n") (t ") = ") (it "k") (sub-scr "4") 
                (t " + ") (dt "S") (t "(") (it "n") (t "-1)")))
   'next
   (blank)
   (colorize
    (page-para* (htl-append 
                 (t "Overall, ")
                 (vl-append
                  line-sep
                  (page-para* (hbl-append (bt "S") (t "(") (it "n") (t ")"))
                              "is proportional to" (it "n"))
                  (page-para* (hbl-append (bt "T") (t "(") (it "n") (t ")"))
                              "is proportional to" (hbl-append (it "n") (super-scr "2"))))))
    BlueColor))

  (define (aq-sequence title cmds . fish)
    (let ([a (list (colorize (page-para title) GreenColor)
                   (let loop ([cmds cmds][cols '("red" "green" "blue")])
                     (cond
                       [(null? (cddr cmds))
                        (mk-fish (car cols) (car cmds) (cadr cmds))]
                       [(eq? (car cmds) 'space)
                        (hc-append gap-size (blank gap-size) (loop (cdr cmds) cols))]
                       [else
                        (hc-append gap-size 
                                   (mk-fish (car cols) (car cmds) (cadr cmds))
                                   (loop (cddr cmds) (cdr cols)))])))])
      (cons a
            (if (null? fish)
                null
                (apply aq-sequence title fish)))))
  
  (define real-fish-seq
    (aq-sequence (page-para "Real fish:")
                 (list 3 #t 2 #t 3 #t)
                 (list 3 #f 2 #t 3 #t)
                 (list 3 #f 5 #f 3 #t)
                 (list 3 #f 5 #f 8 #f)))
  
  (slide/title
   "How Much a Food Chain should Cost"
   (page-para "With 100 fish, our"
              (code food-chain)
              "takes 10,000 steps to feed all the fish")
   'next
   (colorize
    (page-para* "Real fish are clearly more efficient!")
    BlueColor)
   (blank)
   'alts
   (list (list 'alts
               real-fish-seq)
         (list 'alts
               (aq-sequence (page-para "Our algorithm:")
                            (list 3 #t 2 #t 3 #t)
                            (list 3 #t 'space 2 #t 3 #t)
                            (list 3 #t 'space 2 #t 'space 3 #t)
                            (list 3 #t 'space 2 #t 'space 3 #f)
                            (list 3 #t 'space 2 #t 'space 3 #t)
                            (list 3 #t 'space 2 #t 'space 5 #f)
                            (list 3 #t 'space 2 #f 5 #f)
                            (list 3 #t 'space 2 #t 5 #t)
                            (list 3 #t 'space 5 #f 5 #t)
                            (list 3 #t 'space 5 #f 8 #f)
                            (list 3 #f 5 #f 8 #f)))))
              
  (slide/title
   "Practical Feeding"
   (page-para "With real fish, eating" (dt "accumulates")
              "a bigger fish while progressing up the chain:")
   'alts
   real-fish-seq
   'next
   (blank)
   (colorize
    (page-para "Let's imitate this in our function")
    BlueColor)
   (code (code:contract food-chain-on 
                        code:blank : list-of-num num -> list-of-num)
         (code:comment "Feeds fish in l to each other,")
         (code:comment "starting with the fish so-far")
         (define (food-chain-on l so-far) ...)))
  
  (define food-chain-on-defn
    (code
     (define (food-chain-on l so-far)
       (cond
         [(empty? l) empty]
         [else 
          (cons (+ so-far (first l))
                (food-chain-on 
                 (rest l)
                 (+ so-far (first l))))]))))
    
  (slide/title
   "Accumulating Food"
   (vl-append
    line-sep
    food-chain-on-defn
    (code code:blank
          (define (food-chain l)
            (food-chain-on l 0))))
   'next
   (colorize (hline (* 3/4 client-w) 1) GreenColor)
   'alts
   (list (list (vl-append line-sep
                          (code (food-chain '(3 2 3)))
                          (page-para sym:rightarrow)
                          (code (food-chain-on '(3 2 3) 0))))
         (list (vl-append line-sep
                          (code (food-chain-on '(3 2 3) 0))
                          (page-para sym:rightarrow sym:rightarrow)
                          (code (cons 3 (food-chain-on '(2 3) 3)))))
         (list (vl-append line-sep
                          (code (cons 3 (food-chain-on '(2 3) 3)))
                          (page-para sym:rightarrow sym:rightarrow)
                          (code (cons 3 (cons 5 (food-chain-on '(3) 5))))))
         (list (vl-append line-sep
                          (scale/improve-new-text
                           (code (cons 3 (cons 5 (cons 8 (food-chain-on empty 8)))))
                           0.9)
                          (page-para sym:rightarrow sym:rightarrow)
                          (code (cons 3 (cons 5 (cons 8 empty))))))))
         

  (slide/title
   "Accumulators"
   food-chain-on-defn
   (blank)
   (page-para "The" (code so-far) "argument of" (code food-chain-on)
              "code is an" (dt "accumulator")))

  (slide/title
   "The Direction of Information"
   (page-para "With structural recusion, information from"
              "deeper in the structure is returned to computation shallower in the structure")
   (code (define (fun-for-loX l)
           (cond
             [(empty? l) ...]
             [else
              ... (first l)
              ... (fun-for-loX (rest l)) ...]))))
  (slide/title
   "The Direction of Information"
   (page-para "An accumulator sends information the other way"
              sym:emdash "from shallower in the structure to deeper")
   (code (define (acc-for-loX l accum)
           (cond
             [(empty? l) ...]
             [else
              ... (first l) ... accum ...
              ... (acc-for-loX 
                   (rest l) 
                   ... accum ... (first l) ...) 
              ...]))))
  
  (slide/title/center
   "Another Example: Reversing a List"
   (problem "Implement" (code reverse-list)
            "which takes a list and returns a new list with the same items"
            "in reverse order")
   (blank)
   (colorize
    (page-para/r "Pretend that" (code reverse) "isn't built in")
    RedColor)
   'next
   (blank)
   (blank)
   (code (code:contract reverse-list : list-of-X -> list-of-X)
         code:blank
         (reverse-list empty) "should be" empty
         (reverse-list '(a b c)) "should be" '(c b a)))
  
  (slide/title
   "Implementing Reverse"
   (page-para "Using the template:")
   (code (define (reverse-list l)
           (cond
             [(empty? l) empty]
             [else
              ... (first l) ...
              ... (reverse-list (rest l)) ...])))
   'next
   (blank)
   (page-para "Is" (code (reverse-list '(b c))) "useful for computing"
              (code (reverse-list '(a b c))) "?")
   'next
   (blank)
   (colorize
    (page-para* (bt "Yes") ": just add" (code 'a) "to the end")
    BlueColor))
  
  (define rev-defn
    (code (define (reverse-list l)
            (cond
              [(empty? l) empty]
              [else
               (snoc (first l)
                     (reverse-list (rest l)))]))))
  
  (define snoc-defn
    (code (define (snoc a l)
            (cond
              [(empty? l) (list a)]
              [else
               (cons (first l)
                     (snoc a (rest l)))]))))
  
  (slide/title
   "Implementing Reverse"
   (vl-append
    line-sep
    rev-defn
    (code code:blank)
    snoc-defn
    (code code:blank
          (snoc 'a '(c b)) "should be" '(c b a))))

  (slide/title
   "The Cost of Reversing"
   (page-para "How long does" (code (reverse l)) "take when"
              (code l) "has" (it "n") "items?")
   'next
   (blank)
   rev-defn
   'next
   (blank)
   (colorize
    (vl-append
     line-sep
     (page-para* "This is just like the old" (code food-chain) sym:emdash)
     (page-para* "it takes time proportional to" 
                 (hbl-append (it "n") (super-scr "2"))))
    BlueColor))
   
  (slide/title
   "Reversing More Quickly"
   'alts
   (list
    (list
     (vl-append line-sep
                (code (reverse-list '(a b c)))
                (hbl-append gap-size sym:rightarrow sym:rightarrow)
                (code (snoc 'a (reverse-list '(b c))))
                (hbl-append gap-size sym:rightarrow sym:rightarrow)
                (code (snoc 'a '(c b)))
                (code ...))
     (blank)
     (page-para "We could avoid the expensive" (code snoc)
                "step if only we knew to start the result of"
                (code (reverse-list '(c b))) "with"
                (code '(a)) "instead of" (code empty)))
    (list
     (vl-append line-sep
                (code (reverse-list '(a b c)))
                (hbl-append gap-size sym:rightarrow sym:rightarrow)
                (code (reverse-onto '(b c) '(a)))
                (code ...))
     (blank)
     (page-para "It looks like we'll just run into the same problem"
                "with" (code 'b) "next time around..."))
    (list (vl-append line-sep
                     (code (reverse-list '(a b c)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto '(b c) '(a)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (snoc 'b (reverse-onto '(c) '(a))))
                     (code ???))
          (blank)
          (page-para "But this isn't right anyway:" (code 'b)
                     "is supposed to go before" (code 'a))
          (page-para "Really we should reverse" (code '(c))
                     "onto" (code '(b a))))
    (list (vl-append line-sep
                     (code (reverse-list '(a b c)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto '(b c) '(a)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto '(c) '(b a)))
                     (code ...))
          (blank)
          (page-para "And the starting point is that we reverse onto"
                     (code empty) "..."))
    (list (vl-append line-sep
                     (code (reverse-list '(a b c)))
                     sym:rightarrow
                     (code (reverse-onto '(a b c) empty))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto '(b c) '(a)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto '(c) '(b a)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code (reverse-onto empty '(c b a)))
                     (hbl-append gap-size sym:rightarrow sym:rightarrow)
                     (code '(c b a)))
          (blank)
          (page-para "The second argument to" (code reverse-onto)
                     (dt "accumulates") "the answer"))))
  
  (slide/title
   "Accumulator-Style Reverse"
   (code (code:contract reverse-onto : 
                        code:blank list-of-X list-of-X -> list-of-X)
         (define (reverse-onto l base)
           (cond
             [(empty? l) base]
             [else (reverse-onto (rest l)
                                 (cons (first l)
                                       base))]))
         code:blank
         (define (reverse-list l)
           (reverse-onto l empty))))

  (slide/title
   "Foldl"
   (page-para "Remember" (code foldr) ", which is an abstraction of the template?")
   (page-para "The pure accumulator version is" (code foldl) ":")
   (code (code:contract foldl : (X Y -> Y) Y list-of-X -> Y)
	 (define (foldl ACC accum l)
	   (cond
	    [(empty? l) accum]
	    [else (foldl ACC
			 (ACC (first l) accum)
			 (rest l))]))
	 code:blank
	 (define (reverse-list l)
	   (foldl cons empty l))))

  )
