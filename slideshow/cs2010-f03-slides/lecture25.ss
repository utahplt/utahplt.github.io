#cs
(module lecture25 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/eval-step.ss"
           "utils/run.ss"
	   "utils/comm.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  
  (with-steps
   (ques file network ques2)
   (slide/title
    "Multiple Programs"
    (page-para "How do programs communicate?"
               ((vafter file) (t "Files..."))
               ((vafter network) (t "Network..."))
               ((vafter ques2) (t "Etc.")))
    (comm-machine (vonly file) (vafter network))
    (blank)
    ((vafter ques2) (page-para "But what's in a file or sent over the network?"))))

  (define up (colorize (arrow gap-size (* pi 1/2)) RedColor))
  
  (define (mk-file content index)
    (let* ([i (map tt content)]
           [p (apply hbl-append (pict-width (tt " ")) i)])
      (let ([p (let-values ([(x y) (find-cb p (list-ref i index))])
                 (vl-append
                  p
                  (inset up (- x (/ (pict-width up) 2)) 0 0 0)))])
        (cc-superimpose
         (file-icon (+ gap-size (pict-width p))
                    (* 2 (pict-height p))
                    #t)
         p))))
  
  (with-steps
   (init read1 read2)
   (slide/title
    "Byte Streams"
    (page-para "Operating systems provide files, network connections, etc. as" 
	       (dt "byte stream") "objects")
    (blank)
    (page-para "A" (dt "byte") "is a number between" (code 0) "and" (code 255))
    (page-para "A" (dt "stream") "is a sequence with a pointer and an operation:" (code read) "or" (code write))
    (mk-file '("104" "101" "108" "108" "111")
             (cond
               [(only? init) 0]
               [(only? read1) 1]
               [(only? read2) 2]))
    ((vafter read1)
     (page-para (code (read i)) sym:rightarrow (code 104)))
    ((vafter read2)
     (page-para (code (read i)) sym:rightarrow (code 101)))))

  (define network (comm-machine (lambda (x) (blank)) values))
  
  (with-steps
   (init write1 write2 read1 read2)
   (slide/title
    "Byte Streams and Networks"
    (hc-append
     gap-size
     (vl-append line-sep
                ((vafter write1)
                 (page-para* (code (write 104 o))))
                ((vafter write1)
                 (page-para* " " sym:rightarrow (code (void))))
                (blank font-size)
                ((vafter write2)
                 (page-para* (code (write 101 o))))
                ((vafter write2)
                 (page-para* " " sym:rightarrow (code (void)))))
     (vc-append
      gap-size
      network
      (cond
        [(or (only? init) (only? read2)) (tt " ")]
        [(only? write1) (tt "104")]
        [(only? read1) (tt "101")]
        [(only? write2) (tt "101  104")]))
     (vl-append line-sep
                ((vafter read1)
                 (page-para* (code (read i))))
                ((vafter read1)
                 (page-para* " " sym:rightarrow (code 104)))
                (blank font-size)
                ((vafter read2)
                 (page-para* (code (read i))))
                ((vafter read2)
                 (page-para* " " sym:rightarrow (code 101)))))))
  
  (slide/title
   "Encoding"
   (page-para "To communicate information other than small numbers, it must be" (dt "encoded"))
   (blank)
   (page-para "To encode English text, map each" (dt "character") "to a byte")
   (table 3
          (list (code #\a) sym:implies (code 97)
                (code #\b) sym:implies (code 98)
                (code #\c) sym:implies (code 99)
                (blank) (t "...") (blank)
                (code #\A) sym:implies (code 65)
                (blank) (t "...") (blank)
                (code #\() sym:implies (code 40)
                (code #\)) sym:implies (code 41)
                (code #\1) sym:implies (code 48)
                (blank) (t "...") (blank)
                )
          cc-superimpose cc-superimpose
          (* 2 gap-size) line-sep))
  
  (with-steps
   (init read1 read2)
   (slide/title
    "Character Streams"
    (page-para "This character encoding is so popular that byte streams" 
               "are sometimes viewed as" (dt "character streams"))
    (blank)
    (mk-file '("#\\h" "#\\e" "#\\l" "#\\l" "#\\o")
             (cond
               [(only? init) 0]
               [(only? read1) 1]
               [(only? read2) 2]))
    ((vafter read1)
     (page-para (code (read-char i)) sym:rightarrow (code #\h)))
    ((vafter read2)
     (page-para (code (read-char i)) sym:rightarrow (code #\e)))))

  (slide/title
   "Character Streams in Scheme"
   (code
    (define o (open-output-file "ex1"))
    (write-char #\h o)
    (write-char #\e o)
    ...
    (close-output-port o)
    code:blank
    (define i (open-input-file "ex1"))
    (read-char i) "should be" #\h
    (read-char i) "should be" #\e
    ...
    (close-input-port i))
   (blank)
   (blank)
   (colorize
    (page-para* "Note: Scheme term for" (dt "stream") "is" (dt "port"))
    BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (grammar-line . l)
    (apply page-para* (blank (* 2 gap-size) 1) l))
  
  (define (grammar-blank) (blank 1 (/ font-size 2)))

  (slide/title
   "Communicating More Than Characters"
   (page-para (code read-char) "and" (code write-char)
              "are sufficient for communicating character sequences"
              "(or small-number sequences)")
   (page-para "To read and write aquariums, we need to communicate lists of (large) numbers")
   'next
   (blank)
   (page-para "One again, we must encode:")
   (scale/improve-new-text
    (table 3
           (list
            (code empty) sym:implies (code #\.)
            (code '(10000)) sym:implies (code #\1 #\0 #\0 #\0 #\space #\.)
            (code '(1 2)) sym:implies (code #\1 #\space #\2 #\space #\.)
            (blank) (t "...") (blank))
           lc-superimpose lc-superimpose
           (* 2 gap-size) line-sep)
    0.9))
  
  (slide/title
   "Number List Example"
   (vl-append
    line-sep
    (page-para* "A" (code <numlist>) "is")
    (grammar-line (tt "#\\."))
    (grammar-line (code <num>) (tt "#\\space") (code <numlist>))
    (grammar-blank)
    (page-para* "A" (code <num>) "is")
    (grammar-line (code <digit>))
    (grammar-line (code <num>) (code <digit>))
    (grammar-blank)
    (page-para* "A" (code <digit>) "is")
    (grammar-line (tt "#\\0"))
    (grammar-line (tt "#\\1"))
    (grammar-line (it "..."))
    (grammar-line (tt "#\\9"))))
  
  (define numlist-grammar-slide (most-recent-slide))

  (define-code/string/scale
   0.7
   (numlist-out-run numlist-out-code numlist-out-string)
   (code:contract write-numlist : list-of-num output-port -> void)
   (define (write-numlist l p)
     (cond
       [(empty? l) (write-char #\. p)]
       [else (begin
               (write-num (first l) p)
               (write-char #\space p)
               (write-numlist (rest l) p))]))
   code:blank
   (code:contract write-num : num output-port -> void)
   (define (write-num n p)
     (cond
       [(< n 10) (write-digit n p)]
       [else (begin
               (write-num (quotient n 10) p)
               (write-digit (remainder n 10) p))]))
   code:blank
   (code:contract write-digit : num[0-9] output-port -> void)
   (define (write-digit n p)
     (cond
       [(= n 0) (write-char #\0 p)]
       _
       [(= n 1) (write-char #\1 p)]
       _
       [(= n 2) (write-char #\2 p)]
       _
       [(= n 3) (write-char #\3 p)]
       _
       [(= n 4) (write-char #\4 p)]
       _
       [(= n 5) (write-char #\5 p)]
       _
       [(= n 6) (write-char #\6 p)]
       _
       [(= n 7) (write-char #\7 p)]
       _
       [(= n 8) (write-char #\8 p)]
       ...
       [(= n 9) (write-char #\9 p)])))
  
  (slide/title/tall
   "Number List Writer"
   (cb-superimpose
    numlist-out-code
    (mk-copy numlist-out-string)))

  (re-slide numlist-grammar-slide)
  
  (slide/title
   "Number List Example"
   (page-para "Parsing algorithms" sym:implies "use the following equivalent form:")
   (vl-append
    line-sep
    (page-para* "A" (code <numlist>) "is")
    (grammar-line (tt "#\\."))
    (grammar-line (tt "#\\0") (code <num>) (code <numlist>))
    (grammar-line (it "..."))
    (grammar-line (tt "#\\9") (code <num>) (code <numlist>))
    (grammar-blank)
    (page-para* "A" (code <num>) "is")
    (grammar-line (tt "#\\space"))
    (grammar-line (tt "#\\0") (code <num>))
    (grammar-line (it "..."))
    (grammar-line (tt "#\\9") (code <num>))))

  (define-code/string/scale
   0.7
   (numlist-run numlist-code numlist-string)
   (code:contract read-numlist : input-port -> list-of-num)
   (define (read-numlist p)
     (local [(define c (read-char p))]
       (cond
         [(char=? #\. c) empty]
         [(char-digit? c) (cons (read-number p (digit-val c))
                                (read-numlist p))])))
   code:blank
   (code:contract read-number : input-port num -> num)
   (define (read-number p n)
     (local [(define c (read-char p))]
       (cond
         [(char=? #\space c) n]
         [(char-digit? c)
          (read-number p (+ (* n 10) (digit-val c)))])))
   code:blank
   (code:contract char-digit? : char -> bool)
   ...
   _
   (define (char-digit? c)
     (or (char=? c #\0) (char=? c #\1) (char=? c #\2)
         (char=? c #\3) (char=? c #\4) (char=? c #\5)
         (char=? c #\6) (char=? c #\7) (char=? c #\8)
         (char=? c #\9)))
   code:blank
   (code:contract digit-val : char -> num)
   ...
   _
   (define (digit-val c)
     (cond
       [(char=? c #\0) 0]
       [(char=? c #\1) 1]
       [(char=? c #\2) 2]
       [(char=? c #\3) 3]
       [(char=? c #\4) 4]
       [(char=? c #\5) 5]
       [(char=? c #\6) 6]
       [(char=? c #\7) 7]
       [(char=? c #\8) 8]
       [(char=? c #\9) 9])))

  (slide/title
   "Number List Reader"
   (cb-superimpose
    numlist-code
    (mk-copy numlist-string)))
  
  (slide/title
   "read and write"
   (page-para "That's the idea, but you usually don't have to start from scratch")
   (blank)
   (page-item "Built into Scheme:" (code read) "and" (code write))
   (page-subitem "Like" (code read-from-string) ", but handles strings, chars, etc.")
   (blank)
   (page-item "Next time:" (code read-xml) "and" (code write-xml))
   (page-subitem "A generalization of HTML")
   (blank)
   (colorize
    (page-para* "Using read/write libraries means easier encoding")
    BlueColor))
  
  (define-code/string/scale
   0.70
   (ft-run ft-code ft-string)
   (code:comment "A family-tree is either")
   (code:comment " - empty")
   (code:comment " - (make-child family-tree family-tree sym)")
   (define-struct child (father mother name))
   code:blank
   (define MY-FAMILY (make-child empty empty 'Matthew))
   code:blank
   _
   (code:comment "----------------------------------------")
   _
   code:blank
   (code:contract add-mother! : sym sym -> void)
   (define (add-mother! c-name m-name)
     (set! MY-FAMILY (add-mother MY-FAMILY c-name m-name)))
   _
   code:blank
   _
   (code:contract add-father! : sym sym -> void)
   _
   (define (add-father! c-name f-name)
     (set! MY-FAMILY (add-father MY-FAMILY c-name f-name)))
   code:blank
   _
   (code:comment "----------------------------------------")
   _
   code:blank
   (code:contract add-mother : family-tree sym sym -> family-tree)
   ...
   _
   (define (add-mother ft cn mn)
     (add-parent ft cn 
                 (lambda (f m n)
                   (make-child f 
                               (make-child empty empty mn)
                               n))))
   _
   code:blank
   _
   (code:contract add-father : family-tree sym sym -> family-tree)
   _
   (define (add-father ft cn fn)
     (add-parent ft cn 
                 (lambda (f m n)
                   (make-child (make-child empty empty fn)
                               m 
                               n))))
   _
   code:blank
   _
   (code:contract add-parent : family-tree sym
                  code:blank (family-tree family-tree sym -> family-tree)
                  code:blank -> family-tree)
   _
   (define (add-parent ft cn add)
     (cond
       [(empty? ft) empty]
       [else 
        (cond
          [(symbol=? cn (child-name ft))
           (add (child-father ft)
                (child-mother ft)
                cn)]
          [else
           (make-child (add-parent (child-father ft) cn add)
                       (add-parent (child-mother ft) cn add)
                       (child-name ft))])]))
   code:blank
   _
   (code:comment "----------------------------------------")
   _
   code:blank
   (code:contract find-relative : sym -> family-tree-or-false)
   (define (find-relative c-name)
     (find-person MY-FAMILY c-name))
   code:blank
   (code:contract find-person : family-tree sym -> family-tree-or-false)
   ...
   _
   (define (find-person ft cn)
     (cond
       [(empty? ft) false]
       [else (cond
               [(symbol=? cn (child-name ft)) ft]
               [else
                (or (find-person (child-father ft) cn)
                    (find-person (child-mother ft) cn))])]))
   _
   code:blank
   _
   (code:comment "----------------------------------------")
   _
   code:blank)


  (slide/title
   "Family Trees"
   (ct-superimpose
    ft-code
    (mk-copy ft-string)))

  (slide/title/tall
   "Writing Family Trees"
   (scale/improve-new-text
    (code
     (code:contract family-tree->sexp : family-tree -> sexp)
     (define (family-tree->sexp ft)
       (cond
         [(empty? ft) '()]
         [else (list (family-tree->sexp (child-father ft))
                     (family-tree->sexp (child-mother ft))
                     (child-name ft))]))
     code:blank
     (family-tree->sexp empty) "should be" '()
     (family-tree->sexp (make-child empty empty 'Matthew))
     "should be" '(() () Matthew)
     (family-tree->sexp 
      (make-child (make-child empty empty 'Raymond) empty 'Matthew))
     "should be" '((() () Raymond) () Matthew)
     code:blank
     (code:contract write-family-tree : family-tree output-port -> void)
     (define (write-family-tree ft p)
       (write (family-tree->sexp ft) p))
     code:blank
     (define o (open-output-port "my tree"))
     (write-family-tree MY-FAMILY o)
     (close-output-port o))
    0.7))

  (slide/title/tall
   "Reading Family Trees"
   (scale/improve-new-text
    (code
     (code:contract sexp->family-tree : sexp -> family-tree)
     (define (sexp->family-tree sexp)
       (cond
         [(empty? sexp) empty]
         [else (make-child
                (sexp->family-tree (first sexp))
                (sexp->family-tree (second sexp))
                (third sexp))]))
     code:blank
     (sexp->family-tree '()) "should be" empty
     (sexp->family-tree '(() () Matthew))
     "should be" (make-child empty empty 'Matthew)
     code:blank
     (code:contract read-family-tree : input-port -> family-tree)
     (define (read-family-tree i)
       (sexp->family-tree (read i)))
     code:blank
     (define i (open-input-port "my tree"))
     (set! MY-FAMILY (read-family-tree i) )
     (close-input-port i))
    0.8))
  
  (slide/title/center
   "Summary"
   (page-para (dt "Input/output") "(or" (dt "I/O") "for short)"
              ": files, network, and more")
   (blank)
   (page-item "Output" sym:emdash "choose a representation in terms of an existing writer")
   (page-item "Input" sym:emdash "parse representation from an existing reader")
   (blank)
   (blank)
   (page-para "Base reader/writer (practically all operating systems): bytes")
   (page-para/r "... but there are always better libraries"))


  )
