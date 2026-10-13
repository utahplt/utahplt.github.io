(module lecture24 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
           "utils/gcpict.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Interpreter with Continuations"
   (prog*
    "(define (eval-expression exp env cont)"
    "  (cases expression exp"
    "    (lit-exp (datum) "
    "      (apply-cont cont datum))"
    "    (var-exp (id) "
    "      (apply-cont cont (apply-env env id)))"
    "    (proc-exp (id body-exp)"
    "      (apply-cont cont "
    "                (closure id body-exp env)))"
    "    ...))"
    "  "
    "(define (apply-cont cont val)"
    "  (cases continuation cont"
    "    (done-cont () val) "
    "    ...))"))
  
  (slide/title/tall
   "Continuations and Gotos"
   (prog$*
    "(\"goto\")|(;; registers:)"
    "(define (eval-expression exp env cont)"
    " (cases exp ..."
    "  (proc-exp (id body-exp)"
    "    (apply-cont cont "
    "		(closure id body-exp env)))"
    " "
    "=>"
    " "
    ";; registers:"
    "(define EXP ...) (define CONT ...) ..."
    " "
    "(define (eval-expression)"
    " (cases EXP ..."
    "  (proc-exp (id body-exp)"
    "    (set! VAL (closure id body-exp ENV))"
    "    ;; CONT stays the same"
    "    (apply-cont)) ; \"goto\""))
  
  
  (slide/title/tall
   "Continuations and Gotos"
   (prog$*
    "\"goto\""
    "(define (eval-expression exp env cont)"
    " (cases exp..."
    "  (app-exp (rator rand)"
    "    (eval-expression "
    "     rator env"
    "     (app-arg-cont rand env cont)))"
    " "
    "=>"
    " "
    "(define (eval-expression)"
    " (cases EXP ..."
    "  (app-exp (rator rand)"
    "    (set! EXP rator)"
    "    ;; ENV stays the same"
    "    (set! CONT (app-arg-cont rand ENV CONT))"
    "    (eval-expression)) ; \"goto\""))
  
  (slide/title
   "Continuations and Gotos"
   (page-item "Registers and gotos explain why the following"
              "program never generates a stack overflow:")
   (alg-code*
    "let f = proc(f) proc(n) ((f f) n)"
    " in ((f f) 0)")
   'next
   (blank)
   (page-item "So, can we compute arbitrarily deep recursions?")
   (alg-code*
    "let f = proc(f)"
    "          proc(n)"
    "            if n then +(1, ((f f) -(n, 1))) "
    "                 else 0"
    " in ((f f) 1000000000)")
   'next
   (page-para (colorize (bt "No...") RedColor)))
  
  (slide/title
   "Allocation"
   (page-item "We've avoided stack allocation")
   (page-item "But we still have to allocate")
   'next
   (page-subitem "continuation records")
   (page-subitem "closures")
   (page-subitem "environment records"))
  
  
  (define (malloc m)
    (slide/title
     "Allocation"
     (page-item "Where do we call" (tt "malloc") "?")
     (prog$*
      m
      "(define (eval-expression)"
      " (cases EXP ..."
      "  (proc-exp (id body-exp)"
      "    (set! VAL (closure id body-exp ENV))"
      "    ;; CONT stays the same"
      "    (apply-cont))"
      "  (app-exp (rator rand)"
      "    (set! EXP rator)"
      "    ;; ENV stays the same"
      "    (set! CONT (app-arg-cont rand ENV CONT))"
      "    (eval-expression))"
      "  ...")))
  
  (malloc #f)
  (malloc "closure|app-arg-cont")
  
  (slide/title
   "Exposing Allocation"
   (prog$*
    "malloc"
    "(define (closure id body env)"
    "  (let ([v (malloc 4)])"
    "    (mem-set! v 0 closure-tag)"
    "    (mem-set! v 1 id)"
    "    (mem-set! v 2 body)"
    "    (mem-set! v 3 env)"
    "    v))"
    " "
    "(define (closure? v)"
    "  (= (mem-ref v 0) closure-tag))"
    " "
    "(define (closure->id v)"
    "  (mem-ref v 1))"
    "..."))
  
  
  (slide/title
   "Memory Allocator"
   (prog*
    "(define memory (make-vector 200))"
    "(define allocated 0)"
    " "
    "(define (malloc size)"
    "  (let ([result allocated])"
    "    (set! allocated (+ allocated size))"
    "    result))"
    " "
    "(define (mem-set! a n v)"
    "  (vector-set! memory (+ a n) v))"
    " "
    "(define (mem-ref a n)"
    "  (vector-ref memory (+ a n)))"))
  
  (slide/title
   "Exposing Allocation"
   (page-item "Use of" (prog "malloc") "explains why the following program runs out of memory:")
   (alg-code*
    "let f = proc(f)"
    "          proc(n)"
    "            if n then +(1, ((f f) -(n, 1)))"
    "                 else 0"
    " in ((f f) 1000000000)")
   'next
   (blank)
   (page-item "Each call to" (alg-code "(f f)")
              "extends the continuation")
   (page-item "Eventually, the continuation fills all memory"))
  
  (slide/title
   "Exposing Allocation"
   (page-item "Does the following program run forever?")
   (alg-code*
    "let f = proc(f) proc(n) ((f f) n)"
    " in ((f f) 0)")
   'next
   (blank)
   (page-item "Each call to" (alg-code "(f f)"))
   (page-subitem "creates an extended environment")
   (page-subitem "creates a new closure")
   'next
   (blank)
   (page-para* "We need" (dt "deallocation")))
  
  (define (free extra1 extra2 p)
    (slide/title
     "Deallocation"
     (page-item "Where do we call" (tt "free") "?")
     (apply
      prog$*
      p 
      (append
       (list
        "(define (apply-cont)"
        "  (cond ..."
        "   [(app-cont? CONT)"
        "    (let ([rator (app-cont->rator CONT)]"
        "          [old-cont (app-cont->cont CONT)])"
        "      (set! EXP (closure->body rator))")
       extra1
       (list
        "      (set! ENV (extend-env "
        "		 (closure->id rator)"
        "		 VAL"
        "		 (closure->env rator)))")
       extra2
       (list
        "      (set! CONT old-cont))"
        "    (eval-expression)]"
        "  ...")))))
  
  (free null null #f)
  (unless condense?
    (free null 
          (list "      (free CONT)")
          ".free CONT."))
  (free null 
        (list "      (free CONT) ;; unless letcc'd!")
        ".free CONT.|unless.*")
  (unless condense?
    (free (list "      (free ENV)")
          null
          ".free ENV."))
  (free (list "      (free ENV) ;; unless in a closure!")
        null
        ".free ENV.|unless.*")
  
  (slide/title
   "Reference Counting"
   (page-para (dt "Reference counting:")
              "a way to know whether a record has other users")
   'next
   (blank)
   (page-item "Attatch a count to every record, start at 0")
   (page-item "When installing a pointer to a record"
              "(into a register, or another record), "
              "increment its count")
   (page-item "When replacing a pointer to a record, "
              "decrement its count")
   (page-item "When a count is decremented to 0, decrement"
              "counts for other records referenced by the record, "
              "then free it"))
  
  (define ann-w (* client-w 0.45))
  
  (define (ann-item . l)
    (apply item ann-w l))
  
  (define (make-gc-picture/ann a b c d . ann)
    (let ([w ann-w])
      (ht-append
       (cc-superimpose
        (blank w 0)
        (make-gc-picture a b c d))
       (cc-superimpose
        (blank w 0)
        (apply
         vl-append
         font-size
         ann)))))
         
  (slide/title
   "Reference Counting"
   'alts
   (list (list (make-gc-picture/ann
                '(rc) '() null #f
                (ann-item "Top boxes are the registers"
                          (prog "ENV") "," (prog "CONT") ", etc." )
                (ann-item "Boxes in the blue area are allocated with"
                          (prog "malloc"))))
         (list (make-gc-picture/ann
                '(rc moved) '() null #f
                (ann-item "Adjust counts when a pointer is changed...")))
         (list (make-gc-picture/ann
                '(rc moved freed) '() null #f
                (ann-item "... freeing a record if its count goes to 0")))
         (list (make-gc-picture/ann
                '(rc moved freed gone) '() null #f
                (ann-item "Same if the pointer is in a register")))
         (list (make-gc-picture/ann
                '(rc moved freed gone dfreed) '() null #f
                (ann-item "Adjust counts after frees, too...")))
         (list (make-gc-picture/ann
                '(rc moved freed gone dfreed ffreed) '() null #f
                (ann-item "... which can trigger more frees")))
         (list (make-gc-picture/ann
                '(rc moved freed) '() null #f
                (ann-item "Another example")))
         (list (make-gc-picture/ann
                '(rc moved freed cycle) '() null #f
                (ann-item "Adding a reference increments a count")))
         (list (make-gc-picture/ann
                '(rc moved freed cycle gone) '() null #f
                (ann-item "Lower-left records are inaccessible, but not deallocated")
                (ann-item "In general, cycles break reference counting")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Garbage Collection"
   (page-para (bit "Garbage collection:")
              "a way to know whether a record is"
              (it "accessible"))
   'next
   (blank)
   (page-item "A record referenced by a register is" (bit "live"))
   (page-item "A record referenced by a live record is also live")
   (page-item "A program can only possibly use live records, because"
              "there is no way to get to other records")
   'next
   (blank)
   (page-item "A garbage collector frees all records that are not live")
   (page-item "We'll allocate until we run out of memory, then"
              "run a garbage collector to get more space"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Garbage Collection Algorithm"
   (page-item "Color all records" (bit "white"))
   (page-item "Color records referenced by registers" (bit "gray"))
   (page-item "Repeat until there are no gray records:")
   (page-subitem "Pick a gray record," (it "r"))
   (page-subitem "For each white record that" (it "r") "points to,"
                 "make it gray")
   (page-subitem "Color" (it "r") (bit "black"))
   (page-item "Deallocate all white records"))
  
  
  (slide/title
   "Garbage Collection"
   'alts
   (list (list (make-gc-picture/ann
                '(gone cycle moved) '() '() #f
                (ann-item "All records are marked white")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(b) '() #f
                (ann-item "Mark records referenced by registers as gray")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(b) '() 'b
                (ann-item "Need to pick a gray record")
                (ann-item "Red arrow indicates the chosen record")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(b a e) '() 'b
                (ann-item "Mark white records referenced by chosen record as gray")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(a e) '(b) 'b
                (ann-item "Mark chosen record black")))
         
         (list (make-gc-picture/ann
                '(gone cycle moved) '(a e) '(b) 'a
                (ann-item "Start again: pick a gray record")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(e) '(b a) #f
                (ann-item "No referenced records; mark black")))
         
         (list (make-gc-picture/ann
                '(gone cycle moved) '(e) '(b a) 'e
                (ann-item "Start again: pick a gray record")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '(e g) '(b a) 'e
                (ann-item "Mark white records referenced by chosen record as gray")))
         (list (make-gc-picture/ann 
                '(gone cycle moved) '(g) '(b a e) #f
                (ann-item "Mark chosen record black")))     
         
         (list (make-gc-picture/ann
                '(gone cycle moved) '(g) '(b a e) 'g
                (ann-item "Start again: pick a gray record")))
         (list (make-gc-picture/ann
                '(gone cycle moved) '() '(b a e g) #f
                (ann-item "No referenced white records; mark black")))                
         
         (list (make-gc-picture/ann
                '(gone cycle moved cyclegone freed) '() '(b a e g) #f
                (ann-item "No more gray records; deallocate white records")
                (ann-item "Cycles" 
                          (bit "do not")
                          "break garbage collection")))))

   ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "Two-Space Copying Collectors"
   (page-para "A" (dt "two-space") "copying collector compacts"
              "memory as it collects, making allocation easier.")
   (page-para (bt "Allocator:"))
   (page-item "Partitions memory into" (bit "to-space")
              "and" (bit "from-space"))
   (page-item "Allocates only in" (bit "to-space"))
   (page-para (bt "Collector:"))
   (page-item "Starts by swapping"  (bit "to-space")
              "and" (bit "from-space"))
   (page-item "Coloring gray => copy from" 
              (bit "from-space") "to" (bit "to-space"))
   (page-item "Choosing a gray record => walk once though the new" 
              (bit "to-space") ", update pointers"))

  (define (make-gc-picture/ann2 a b c d ann)
    (rt-superimpose
     (cc-superimpose
      (make-gc-picture a b c d)
      titleless-page)
     ann))
  
  (define (ann-item2 . l)
    (apply para ann-w l))
  
  (slide/title/tall
   "Two-Space Collection"
   'alts
   (list (list (make-gc-picture/ann2
                '(gone cycle moved 2) '() '() #f
                (vc-append
                 line-sep
                 (ann-item2 "Left = from-space")
                 (ann-item2 "Right = to-space"))))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy) '() '() #f
                (ann-item2 "Mark gray = copy and leave forward address")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy) '() '() 'b2
                (ann-item2 "Choose gray by walking through to-space")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy) '() '() 'b2
                (ann-item2 "Mark referenced as gray")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy) '() '() 'a2
                (ann-item2 "Mark black = move gray-choosing arrow")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy) '() '() 'e2
                (ann-item2 "Nothing to color gray; increment the arrow")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy gcopy) '() '() 'e2
                (ann-item2 "Color referenced record gray")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy gcopy) '() '() 'g2
                (ann-item2 "Increment the gray-choosing arrow")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy gcopy gupdate) '() '() 'g2
                (ann-item2 "Referenced is already copied, use forwarding address")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy gcopy gupdate) '() '() #f
                (ann-item2 "Choosing arrow reaches the end of to-space: done")))
         (list (make-gc-picture/ann2
                '(gone cycle moved 2 bcopy aecopy gcopy gupdate 2only) '() '() #f
                (vc-append
                 line-sep
                 (ann-item2 "Right = from-space")
                 (ann-item2 "Left = to-space"))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Two-Space Collection on Vectors"
   (page-item "Everything is a number:")
   (page-subitem "Some numbers are immediate integers")
   (page-subitem "Some numbers are pointers")
   (page-item "An allocated record in memory starts with a tag,"
              "followed by a sequence of pointers and immediate integers")
   (page-subitem "The tag describes the shape"))

  (define (mk-regs n a b)
    (page-para* (t (format "~aRegister 1:" n)) a
                (blank (* 3 font-size) 0) 
                (t (format "~aRegister 2:" n)) b))
  
  (define (mk-ex num? start? to-phase)
    (slide/title
     "Two-Space Vector Example"
     (vl-append
      line-sep
      (page-item "26-byte memory (13 bytes for each space), 2 registers")
      (page-subitem "Tag 1: one integer")
      (page-subitem "Tag 2: one pointer")
      (page-subitem "Tag 3: one integer, then one pointer"))
     (blank)
     (mk-regs "" 
              (if (to-phase . < . 2) (tt "7") (prog "0" "0"))
              (if (to-phase . < . 3) (tt "0") (prog "3" "3")))
     (table
      2
      (append
       (list (t "From:")                (prog "99 .." (format "~a ~a ~a ~a  3  2 10 ~a ~a  2  3  1  4"
                                                              (if (to-phase . > . 2) "99" " 1")
                                                              (if (to-phase . > . 2) " 3" "75")
                                                              (if (to-phase . > . 3) "99" " 2")
                                                              (if (to-phase . > . 3) " 5" " 0")
                                                              (if (to-phase . > . 1) "99" " 3")
                                                              (if (to-phase . > . 1) " 0" " 2"))))
       (if num?
          (list (colorize (t "Addr:") RedColor) (colorize (tt "00 01 02 03 04 05 06 07 08 09 10 11 12") RedColor))
          null)
       (if start?
           (list (blank)                        (colorize (tt " ^     ^     ^        ^        ^      ") RedColor))
           null)
       (if (zero? to-phase)
           null
           (cons (t "To:") 
                 (case to-phase
                   [(1) (list (tt " 0  0  0  0  0  0  0  0  0  0  0  0  0") (blank)
                              (tt " ^"))]
                   [(2) (list (tt " 3  2  2  0  0  0  0  0  0  0  0  0  0") (blank)
                              (tt " ^"))]
                   [(3) (list (tt " 3  2  2  1 75  0  0  0  0  0  0  0  0") (blank)
                              (tt " ^"))]
                   [(4) (list (tt " 3  2  5  1 75  2  0  0  0  0  0  0  0") (blank)
                              (tt "          ^"))]
                   [(5) (list (tt " 3  2  5  1 75  2  0  0  0  0  0  0  0") (blank)
                              (tt "                ^"))]
                   [(6) (list (tt " 3  2  5  1 75  2  3  0  0  0  0  0  0") (blank)
                              (tt "                      ^"))]))))
      lc-superimpose lc-superimpose font-size line-sep)))
  
  (mk-ex #f #f 0)
  (mk-ex #t #f 0)
  (mk-ex #t #t 0)
  (mk-ex #t #t 1)
  (mk-ex #t #t 2)
  (mk-ex #t #t 3)
  (mk-ex #t #t 4)
  (mk-ex #t #t 5)
  (mk-ex #t #t 6)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'done)
