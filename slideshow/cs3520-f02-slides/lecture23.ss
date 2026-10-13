(module lecture23 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   (all-except "utils/alg.ss" steps)
           "utils/kont.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Continuations"
   (page-para "The interpreter now consists of two main functions:")
   (page-item (tt "eval-expression : expr env cont -> expval"))
   (eval-step "1" "{}" "[done]")
   (page-item (tt "apply-cont : value cont -> expval"))
   (kont-step "1" "[done]"))

  (slide/title
   "Control Constructs"
   (page-para "Now that we've made control explicit, we can explore"
              "new control constructs:")
   (page-item "Exceptions")
   (page-item "Threads")
   (page-item "Programmer-accessible continuations"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Exceptions"
   (page-item "Exceptions make error-handling easier and more reliable")
   'alts
   (list (list
          (prog*
           "FILE *f;"
           "f = fopen(\"x.txt\", \"r\");"
           "fread(buffer, 1, 256, f);"
           "/* oops: f might be NULL */"))
         (list
          (page-item "Exceptions can communicate \"out of band\" results more clearly")
          'alts
          (list
           (list
            (alg-code*
             "letrec index(n, l) = % find n in l, -1 if not there"
             "             if null?(l)"
             "              then -1"
             "              else let p = (index n cdr(l))"
             "                         in if zero?(add1(p))"
             "                             then -1"
             "                             else add1(p)"
             "  in index(3, list(1, 2, 4))"))
           (list
            (alg-code*
             "letrec index(n, l) = % find n in l, exn if not there"
             "             if null?(l)"
             "              then raise -1"
             "              else add1(index n cdr(l))"
             "  in try index(3, list(1, 2, 4))"
             "      handle proc(x) -1"))))))
  
  (define (esteps start first . l)
    (let ([arrow (alg-code "->")])
      (table
       3
       (list* start arrow first
              (apply append
                     (map (lambda (x)
                            (list (blank) arrow x))
                          l)))
       lc-superimpose lc-superimpose
       font-size line-sep)))
             
  
  (slide/title
   "Exceptions"
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "....") (blank)
     (blank)           -or- (alg-code "try expr handle expr") (blank)
     (blank)           -or- (alg-code "raise expr") (blank)))
   'next
   (blank)
   'alts
   (list (list
          (esteps (alg-code "raise 0")
                 (it "unhandled exception")))
         (list
          (esteps (alg-code "raise sub1(1)")
                  (alg-code "raise 0")
                  (it "unhandled exception")))
         (list
          (esteps (alg-code "try 10 handle proc(x)x")
                  (alg-code "10")))
         (list
          (vl-append
           (alg-code "try 10 handle let f = proc(x)x in f")
           (esteps (blank)
                   (alg-code "try 10 handle proc(x)x")
                   (alg-code "10"))))
         (list
          (esteps (alg-code "try 10 handle 5")
                  (it "handler not a procedure")))
         (list
          (esteps (alg-code "try raise 10 handle proc(x)x")
                  (alg-code "raise 10")
                  (alg-code "10"))
          'next
          (page-item "Where was the handler kept?"))))

  (slide/title
   "Evaluation with Exceptions: Raise"
   'alts
   (steps
    (eval-step "raise 0" "{}" "[done]")
    (eval-step "0" "{}" "[raise [done]]")
    (kont-step "0" "[raise [done]]")
    (it "unhandled exception")))

  (slide/title
   "Evaluation with Exceptions: Try"
   'alts
   (steps
    (eval-step "try 10 handle proc(x)x" "{}" "[done]")
    (eval-step "proc(x)x" "{}" "[try 10 in {} [done]]")
    (kont-step "<x,x,{}>" "[try 10 in {} [done]]")
    (eval-step "10" "{}" "[handle <x,x,{}> [done]]")
    (kont-step "10" "[handle <x,x,{}> [done]]")
    (kont-step "10" "[done]")))

  (slide/title
   "Evaluation with Exceptions: Handle"
   'alts
   (steps
    (eval-step "try raise 10 handle proc(x)x" "{}" "[done]")
    (eval-step "proc(x)x" "{}" "[try raise 10 in {} [done]]")
    (kont-step "<x,x,{}>" "[try raise 10 in {} [done]]")
    (eval-step "raise 10" "{}" "[handle <x,x,{}> [done]]")
    (eval-step "10" "{}" "[raise [handle <x,x,{}> [done]]]")
    (kont-step "10" "[raise [handle <x,x,{}> [done]]]")
    (eval-step "x" "{x=10}" "[done]")
    (kont-step "10" "[done]")))

  (slide/title
   "Evaluation with Exceptions: In Context"
   'alts
   (steps
    (eval-step "sub1(try add1(add1(raise 10)) handle proc(x)x)" "{}" "[done]")
    (eval-step "try add1(add1(raise 10)) handle proc(x)x" "{}" "[-1 [done]]")
    (eval-step "proc(x)x" "{}" "[try add1(add1(raise 10)) in {} [-1 [done]]]")
    (kont-step "<x,x,{}>" "[try add1(add1(raise 10)) in {} [-1 [done]]]")
    (eval-step "add1(add1(raise 10))" "{}" "[handle <x,x,{}> [-1 [done]]]")
    (eval-step "add1(raise 10)" "{}" "[+1 [handle <x,x,{}> [-1 [done]]]]")
    (eval-step "raise 10" "{}" "[+1 [+1 [handle <x,x,{}> [-1 [done]]]]]")
    (eval-step "10" "{}" "[raise [+1 [+1 [handle <x,x,{}> [-1 [done]]]]]]")
    (kont-step "10" "[raise [+1 [+1 [handle <x,x,{}> [-1 [done]]]]]]")
    (eval-step "x" "{x=10}" "[-1 [done]]")
    (kont-step "10" "[-1 [done]]")
    (kont-step "9" "[done]")))
  
  (slide/title
   "Exceptions: Implementation"
   (page-para "In" (prog "eval-expression") ":")
   (scale/improve-new-text
    (prog*
     "(try-exp (body-exp handler-exp)"
     "  (eval-expression"
     "   handler-exp env (try-cont body-exp env cont))")
    0.9)
   (page-para "In" (prog "apply-cont") ":")
   (scale/improve-new-text
    (prog*
     "(try-cont (body-exp env cont)"
     "  (if (proc? val)"
     "      (eval-expression"
     "       body-exp env (handle-cont val cont))"
     "      (error \"handler not a proc\"))"
     "(handle-cont (handler cont)"
     "   (apply-cont cont val))")
    0.9))
  
  (slide/title
   "Exceptions: Implementation"
   (page-para "In" (prog "eval-expression") ":")
   (prog*
    "(raise-exp (expr)"
    "   (eval-expression"
    "    expr env (raise-cont cont)))")
   (page-para "In" (prog "apply-cont") ":")
   (prog$*
    "find-handler"
    "(raise-cont (cont)"
    "   (find-handler val cont))"))
    
  (slide/title
   "Exceptions: Implementation"
   (prog*
    "(define (find-handler val cont)"
    "  (cases continuation cont"
    "    (handle-cont (handler cont)"
    "       (apply-proc handler val cont))"
    "    (done-cont ()"
    "       (error \"unhandled exception\"))"
    " "
    "    ; All others: look in the rest"
    "    (prim-other-cont (prim arg2 env cont)"
    "       (find-handler val cont)) "
    "    ...."))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Threads"
   (page-item "So far, our languages have been" (dt "single-threaded"))
   (page-item "We can add a" (alg-code "spawn") "form to our language"
              "to make it multithreaded")
   (grammar-table
    (list
     (alg-code "expr") eqls (alg-code "....") (blank)
     (blank)           -or- (alg-code "spawn expr") (blank)
     (alg-code "prim") eqls (alg-code "....") (blank)
     (blank)           -or- (alg-code "print") (blank)))
   (page-item "Note: threads are only useful with side effects"))
  
  (slide/title
   "Threads"
   (page-item "To implement threads, we need some way of packaging up all"
              "information about a computation")
   'next
   (page-item "The arguments to" (prog "eval-expression")
              "and" (prog "apply-cont")
              (it "are") "complete information!")
   'next
   (blank)
   (page-item "So, all we need is a way to keep a queue of"
              "thread states, and a way to switch to a different"
              "state occasionally"))
  
  (slide/title
   "Thread Implementation"
   (page-item "Rename" (prog "eval-expression")
              "to" (prog "do-eval-expression"))
   (page-item "Define a new" (prog "eval-expression") ":")
   (prog*
    "(define (eval-expression exp env cont)"
    "  (if (time-to-swap!?)"
    "      (swap-thread! 'eval (list exp env cont))"
    "      (do-eval-expression exp env cont))))"))

  (slide/title
   "Thread Implementation"
   (page-item "Rename" (prog "apply-cont")
              "to" (prog "do-apply-cont"))
   (page-item "Define a new" (prog "apply-cont") ":")
   (prog*
    "(define (apply-cont cont val)"
    "  (if (time-to-swap!?)"
    "      (swap-thread! 'cont (list cont val))"
    "      (do-apply-cont cont val)))"))
  
  (slide/title
   "Thread Implementation"
   (blank)
   (page-para* "(implement the rest in DrScheme)"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Continuations as Values"
   (page-item "With procedures, a program has a way to grab the current environment"
              "the restore it later")
   (page-item "What if we let programs grab and restore the environment?")
   'next
   (blank)
   (page-para (dt "First-class continuations"))
   'alts
   (list (list 
          (let ([w (/ client-w 2)])
            (vl-append
             line-sep
             (para w "An expressed value is")
             (subitem w "a number")
             (subitem w "a proc")
             (subitem w "a continuation"))))
         (list
          (grammar-table
           (list
            (alg-code "expr") eqls (alg-code "....") (blank)
            (blank)           -or- (alg-code "letcc id in expr") (blank)
            (blank)           -or- (alg-code "continue expr expr") (blank))))))
  
  (slide/title
   "Evaluation with letcc"
   'alts
   (steps
    (eval-step "letcc k +(continue k 2, 1)" "{}" "[done]")
    (eval-step "+(continue k 2, 1)" "{k=[done]}" "[done]")
    (eval-step "continue k 2" "{k=[done]}" "[addexp 1 {k=[done]} [done]]")
    (eval-step "k" "{k=[done]}" "[contexp 2 {k=[done]} [addexp 1 {k=[done]} [done]]]")
    (kont-step "[done]" "[contexp 2 {k=[done]} [addexp 1 {k=[done]} [done]]]")
    (eval-step "2" "{k=[done]}" "[cont [done] [addexp 1 {k=[done]} [done]]]")
    (kont-step "2" "[cont [done] [addexp 1 {k=[done]} [done]]]")
    (kont-step "2" "[done]")))

  (slide/title
   "Evaluation with letcc"
   (page-para "The" (alg-code "cont") "continuation never uses the rest")
   'alts
   (steps
    (kont-step "[done]" "[contexp 2 {k=[done]} [addexp 1 {k=[done]} [done]]]")
    (eval-step "2" "{k=[done]}" "[cont [done]]")
    (kont-step "2" "[cont [done]]")
    (kont-step "2" "[done]")))

  (slide/title
   "Uses for letcc"
   (page-para "First-class continuations are extremely powerful:")
   (page-item (alg-code "letcc") "can be used instead of exceptions")
   (page-item (alg-code "letcc") "can be used to re-play a computation")
   (page-item (alg-code "letcc") "can be used to implement co-operative threads"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)

