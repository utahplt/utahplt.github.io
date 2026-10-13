#cs
(module lecture35 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/run.ss"
           "utils/obj.ss"
           "utils/explain.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           (lib "step.ss" "slideshow")
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "etc.ss"))

  (define outline
    (make-outline
     'for "Loops" #f                    
     'why "When to Use Loops" (lambda (it)
                                (para (* client-w 3/4)
                                      "... and why Java needs a special form for loops"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'for)
  
  (slide/title
   "Iterating with Numbers"
   (page-para "Many computations involve iterating over numbers:")
   (page-item "Checking each item in an array")
   (page-item "Computing sums or products from" (java "0") "to" (java "n"))
   'next
   (blank)
   (page-para "One way to write such loops:")
   (page-item/bullet (bt "Step 1.") "Set" (java "i") "to the starting number")
   (page-item/bullet (bt "Step 2.") "If" (java "i") "is too big, stop")
   (page-item/bullet (ghost (bt "Step 2.")) "Otherwise, do something with" (java "i"))
   (page-item/bullet (bt "Step 3.") "Change" (java "i") "to the next value and go to" (bt "Step 2")))
   
  
  (define sum-code
    (java "int sum(int n) {"
          "  int res = 0;"
          "  for (int i = 0; i <= n; i = i + 1) {"
          "    res = res + i;"
          "  }"
          "  return res;"
          "}"))
  
  (slide/title
   "For Loops"
   (page-para "Java supports this pattern with" (java "for"))
   'alts
   (list (list sum-code)
         (list (explain sum-code
                        2/5 28/70 'nw
                        "Set" (java "i") "to the starting number"))
         (list (explain sum-code
                        3/5 28/70 'n
                        "If" (java "i") "isn't too big..."))
         (list (explain sum-code
                        1/3 38/70 'nw
                        "Do something with" (java "i")))
         (list (explain sum-code
                        4/5 28/70 'ne
                        "Change" (java "i") "to the next value"))))

  (slide/title/center
   "Another Example"
   (java "int sumElements(int[] a) {"
         "  int res = 0;"
         "  for (int i = 0; i < a.length(); i = i + 1) {"
         "    res = res + a[i];"
         "  }"
         "  return res;"
         "}"))

  (slide/title/center
   "Another Example"
   (java "int maxElement(int[] a) {"
         "  int res = a[0];"
         "  for (int i = 1; i < a.length(); i = i + 1) {"
         "    if (res < a[i])"
         "       res = a[i];"
         "  }"
         "  return res;"
         "}"))

  (slide/title/center
   "Another Example"
   (java "int isArrayMember(Object o, Object[] a) {"
         "  for (int i = 0; i < a.length(); i = i + 1) {"
         "    if (o.equals(a[i]))"
         "      return true;"
         "  }"
         "  return false;"
         "}"))

  (slide/title
   "Looping with Values Other than Numbers"
   (page-para "With suitable methods and helpers,"
              "the same pattern can work for list-shaped data:")
   (java "int isListMember(Object o, List lst) {"
         "  for (Enumerator e = lst.elements(); "
         "       e.hasMoreElements(); "
         "       ) {"
         "    Object elem = e.nextElement();"
         "    if (o.equals(elem))"
         "      return true;"
         "  }"
         "  return false;"
         "}"))
  
  (slide/title
   "While Loops"
   (java "while (_test) { ... }")
   (page-para "is a shorthand for")
   (java "for (; _test; ) { ... }")
   (blank)
   (blank)
   (java "int isListMember(Object o, List lst) {"
         "  Enumerator e = lst.elements();"
         "  while (e.hasMoreElements()) {"
         "    Object elem = e.nextElement();"
         "    if (o.equals(elem))"
         "      return true;"
         "  }"
         "  return false;"
         "}"))

  (slide/title
   "Do/While Loops"
   (java "do { ... } while (_test);")
   (page-para "is a shorthand for")
   (java "for (boolean ok=true; ok; ) { ... ok = _test; }")
   (blank)
   (blank)
   (java "int tryUntil(List lst, Tester t) {"
         "  Enumerator e = lst.elements();"
         "  do {"
         "    Object elem = e.nextElement();"
         "  } while (!t.tryIt(elem));"
         "}"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'why)
  
  (slide/title/center
   "When to Use Loops"
   (page-para "Use" (java "for") "," (java "while") "and" (java "do")
              "like you would use" (code filter) "or" (code map))
   (page-subitem "In other words, it's a question of reusing a pattern")
   'next
   (blank)
   (blank)
   (page-para "Using" (code map) "in Scheme is always optional, but"
              "sometimes you really" (it "must") "use" (java "for") "in Java")
   (colorize
    (page-para/r "This is a design flaw in Java that you'll have to live with")
    BlueColor)
   'next
   (blank)
   (page-para "As someone who knows how to design programs, you should understand")
   (page-item "why" (java "for") "is necessary")
   (page-item "how to convert recursive programs to use" (java "for")))
  
  (define (sub-scr s)
    (text s `(subscript . ,main-font) (current-font-size)))
  (define super-scr
    (opt-lambda (s [extra-style null])
      (text s `(,@extra-style superscript . ,main-font) (current-font-size))))

  (slide/title
   "Cost of Computation, Revisited"
   (code (code:contract sum : num -> num)
         (code:comment "Sums the numbers from 0 to n")
         (define (sum n)
           (cond
             [(zero? n) 0]
             [else (+ n (sum (- n 1)))])))
   (blank)
   'alts
   (list
    (list
     (page-para "How long does" (code (sum _n)) "take?")
     'next
     (vc-append
      line-sep
      (hbl-append (bt "T") (t "(0) = k") (sub-scr "1"))
      (hbl-append (bt "T") (t "(") (it "n") (t ") = k") (sub-scr "2") (t " + ") 
                  (bt "T") (t "(") (it "n") (t "-1)")))
     (page-para "So it takes " (hbl-append (t "k") (sub-scr "1") (t " + ")
                                           (t "k") (sub-scr "2") (it "n"))
                ", i.e., proportional to" (it "n")))
    (list
     (page-para "How much" (it "space") "does" (code (sum _n)) "take?")
     'next
     (blank)
     (vl-append (code (sum _n))
                (page-para* sym:rightarrow sym:rightarrow (code (+ _n (sum _n-1))))
                (page-para* sym:rightarrow sym:rightarrow (code (+ _n (+ _n-1 (sum _n-2)))))
                (page-para* sym:rightarrow sym:rightarrow sym:rightarrow (code (+ _n (+ _n-1 (+ _n-2 ... 0))))))
     'next
     (blank)
     (page-para "So it takes space proportional to" (it "n")))))
                

  (slide/title
   "Cost of Computation with an Accumulator"
   (code (code:contract asum : num num -> num)
         (code:comment "Sums the numbers from 0 to n, added to res")
         (define (asum n res)
           (cond
             [(zero? n) res]
             [else (asum (- n 1) (+ res n))])))
   (blank)
   'alts
   (list
    (list
     (page-para "How long does" (code (asum _n 0)) "take?")
     'next
     (page-para "Still proportional to" (it "n")))
    (list
     (page-para "How much" (it "space") "does" (code (asum _n 0)) "take?")
     'next
     (blank)
     (vl-append (code (asum _n 0))
                (page-para* sym:rightarrow sym:rightarrow (code (asum _n-1 _n)))
                (page-para* sym:rightarrow sym:rightarrow (code (asum _n-2 _2n-1)))
                (page-para* sym:rightarrow sym:rightarrow sym:rightarrow 
                            (code (asum 0 #,(hbl-append (code _n)
                                                        (colorize (super-scr "2") id-color)
                                                        (code _/2+n/2))))))
     'next
     (blank)
     (page-para "So it takes constant space, independent of" (it "n")))))
  
  (slide/title
   "Time and Space"
   (page-item "Weeks ago, we saw how an accumulator can save lots of time")
   (page-item "Less frequently, an assumulator can save space (sometimes"
              "even when it saves no time)")
   (page-para "Accumulators save space when the result of a recursive call is the"
              "result for the current call:")
   (code
    (define (asum n res)
      (cond
        [(zero? n) res]
        [else (asum (- n 1) (+ res n))])))
   'next
   (blank)
   (colorize
    (page-para "As it turns out, recursive-call space in Java tends to"
               "be more scarce than other space, so this kind of saving is often"
               "important")
    BlueColor))
             
  (slide/title
   "Cost of Computation in Java"
   (scale/improve-new-text
    (java "class Summer {"
          "  int sum(int n) {"
          "    if (n == 0)"
          "      return 0;"
          "    else"
          "      return n+this.sum(n-1);"
          "  }"
          "}")
    0.9)
   'alts
   (list
    (list
     (page-para "How long does" (java "new Summer.sum(_n)") "take?")
     'next
     (blank)
     (page-para "Still proportional to" (it "n")))
    (list
     (page-para "How much" (it "space") "does" (java "new Summer.sum(_n)") "take?")
     'next
     (vl-append (java "s.sum(_n)")
                (page-para* sym:rightarrow sym:rightarrow (java "return _n+s.sum(__n-1__)")
                (page-para* sym:rightarrow sym:rightarrow (java "return _n+(return __n-1__+s.sum(__n-2__))"))
                (page-para* sym:rightarrow sym:rightarrow sym:rightarrow (java "return _n+(return __n-1__+(return __n-2__+...0))"))))
     'next
     (page-para "Again, space proportional to" (it "n")))))

  (define java-asum-code
    (scale/improve-new-text
     (java "class Summer {"
           "  int asum(int n, int res) {"
           "    if (n == 0)"
           "      return res;"
           "    else"
           "      return this.asum(n-1, res+n);"
           "  }"
           "}")
     0.9))
   
  
  (slide/title
   "Cost of Computation in Java"
   java-asum-code
   'alts
   (list
    (list
     (page-para "How long does" (java "new Summer.asum(_n, 0)") "take?")
     'next
     (blank)
     (page-para "Still proportional to" (it "n")))
    (list
     (page-para "How much" (it "space") "does" (java "new Summer.asum(_n, 0)") "take?")
     'next
     (vl-append (java "s.asum(_n, 0)")
                (page-para* sym:rightarrow sym:rightarrow (java "return s.asum(__n-1__, _n)")
                (page-para* sym:rightarrow sym:rightarrow (java "return return s.asum(__n-2__, __2n-1__))"))
                (page-para* sym:rightarrow sym:rightarrow sym:rightarrow
                            (hbl-append (java "return return ... return _n")
                                        (colorize (super-scr "2") id-color)
                                        (java "__/2+n/2__")))))
     'next
     (page-para (bt "Still") "space proportional to" (it "n") ", due to all the" 
                (hbl-append (java "return") (it "s"))))))
  
  (slide/title
   "Tail Calls in Java"
   java-asum-code
   (page-para "The" (java "return") "explanation reflects the actual semantics of Java:"
              "redundant" (hbl-append (java "return") (it "s")) "do not get dropped")
   'next
   (blank)
   (page-item "To allow constant-space loops, languages like Java provide a special form")
   (page-item "The special form only works for loops with no arguments"))
  
  (slide/title/tall
   "Getting Rid of Arguments"
   (scale/improve-new-text
    (code (define (sum n)
            (local [(define (asum i res)
                      (cond
                        [(zero? i) res]
                        [else (asum (- i 1) (+ res i))]))]
              (asum n 0))))
    0.8)
   'next
   (page-para "Equivalent Scheme code (in extremely poor style):")
   (scale/improve-new-text
    (code (define (sum n)
            (local [(define res 0)
                    (define i n)
                    (define (asum)
                      (cond
                        [(zero? i) (void)]
                        [else (set! res (+ res i))
                              (set! i (- i 1))
                              (asum)]))]
              (asum)
              res)))
    0.8))
  
  (define java-while-loop
    (scale/improve-new-text
     (java "class Summer {"
           "  int sum(int n) {"
           "    int res = 0;"
           "    int i = n;"
           "    while (true) {"
           "      if (i == 0)"
           "        break;"
           "      else {"
           "        res = res + i;"
           "        i = i - 1;"
           "      }"
           "    }"
           "    return res;"
           "  }"
           "}")
     0.8))
  
  (slide/title
   "Loops in Java"
   (ht-append
    gap-size
    (scale/improve-new-text
     (code (define (sum n)
             (||local
              [(define res 0)
               (define i n)
               (define (asum)
                 (cond
                   [(zero? i) (void)]
                   [else (set! res (+ res i))
                         (set! i (- i 1))
                         (asum)]))]
              (asum)
              res)))
     0.8)
    java-while-loop)
   (blank)
   (page-para "The" (java "while") "form is like a recursive function"
              "that always either returns void or calls itself with no arguments"))
            
  (define java-better-while-loop
    (scale/improve-new-text
     (java "class Summer {"
           "  int sum(int n) {"
           "    int res = 0;"
           "    int i = n;"
           "    while (i == 0) {"
           "      res = res + i;"
           "      i = i - 1;"
           "    }"
           "    return res;"
           "  }"
           "}")
     0.8))
  
  (slide/title
   "Loops in Java, Slightly Better Style"
   (vl-append
    line-sep
    (java "while (true) { if (_test) break; else ... }")
    (hbl-append gap-size sym:implies (java "while (_test) { ... }")))
   (blank)
   (htl-append
    (* 3 gap-size)
    java-while-loop
    java-better-while-loop))

  (define java-for-loop
    (scale/improve-new-text
     (java "class Summer {"
           "  int sum(int n) {"
           "    int res = 0;"
           "    for (int i = n;"
           "         i != 0;"
           "         i = i - 1) {"
           "      res = res + i;"
           "    }"
           "  }"
           "}")
     0.8))
  
  (slide/title
   "Loops in Java, Good Style"
   (vl-append
    line-sep
    (java "_init; while (_test) { ... _incr; }")
    (hbl-append gap-size sym:implies (java "for (_init; _test; _incr) { ... }")))
   (blank)
   (htl-append
    (* 3 gap-size)
    java-while-loop
    java-for-loop))
  
  (slide/title/center
   "When Loops Don't Work"
   (page-para "Converting tree recusion into a loop usually won't work,"
              "because there are multiple recursive calls for each call")
   (page-para "Some algorithms, such as" (code merge-sort) ", also involve"
              "multiple recursive calls")
   (blank)
   (blank)
   (page-para "Technically, any program can be converted by manually"
              "creating" (dt "continuations") ", but that's"
              "a topic for CS 3520"))
  
  (slide/title/center
   "Loops in Java"
   (page-para "Conclusion:")
   (page-item "Use" (code for) "and" (code while) "to make your code look and run better")
   (page-item "When in doubt, write the recursive version first"))
              

  )
