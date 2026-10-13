(module lecture22 (lib "slideshow-run.ss" "texpict")
  
  (require "utils/colors.ss"
	   "utils/utils.ss"
	   (all-except "utils/alg.ss" steps)
           "utils/kont.ss")
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/tall
   "Recursion in C"
   (page-para "What does the following program compute?")
   (blank)
   (prog*
    "int f(int n, int v) {"
    "  if (n == 0)"
    "    return v;"
    "  else"
    "    return f(n - 1, v + n);"
    "}"
    " "
    "int main() { return f(1000000, 0); }")
   'next
   (blank)
   (page-para (colorize (t "Answer:") BlueColor) "stack overflow"))
  
  (slide/title/tall
   "Loops in C"
   (page-para "What does the following program compute?")
   (blank)
   (prog*
    "int main() {"
    "  int n = 1000000;"
    "  int v = 0;"
    " "
    "  while (n > 0) {"
    "    v = v + n;"
    "    n = n - 1;"
    "  }"
    " "
    "  return v;"
    "}")
   'next
   (blank)
   (page-para (colorize (t "Answer:") BlueColor)
              "the sum of 0 to 1000000 (in 32-bit two's complement)"))
  
  (slide/title
   "Recursion in the Book Language"
   (page-para "What does the following program compute?")
   (alg-code*
    "letrec f = proc(n, v)"
    "                 if n"
    "                   then (f -(n,1) +(n, v))"
    "                   else v"
    "   in (f 1000000 0)")
   'next
   (blank)
   (page-para (colorize (t "Answer:") BlueColor)
              "the sum of 0 to 1000000"))
  
  (slide/title
   "Recursion in the Book Language"
   (page-para "Why don't we get a stack overflow in the book language?")
   'next
   (page-item "This is actually a question about Scheme:")
   (page-subitem "We implement"
                 "recursion for the book language by using Scheme's recursion")
   (page-subitem "Similarly, we use Scheme numbers to implement numbers for"
                 "the book language")
   (page-item "Such an explanation/implementation is called" (dt "meta-circular"))
   (page-item "We don't care so much about numbers, but we don't want to explain"
              "away recursion; we want to understand recursion (in Scheme and the book language)"))
  
  
  (define (mk-scheme-func pat)
    (prog$*
     pat
     "(define (f n v)"
     "  (if (zero? n)"
     "      v"
     "      (f (- n 1) (+ n v))))"))
  
  (slide/title
   "Recursion in Scheme"
   (page-para "What does the following program compute?")
   (blank)
   (vl-append
    line-sep
    (mk-scheme-func "!")
    (prog*
     " "
     "(f 1000000 0)"))
   'next
   (blank)
   (page-para (colorize (t "Answer:") BlueColor)
              "the sum of 0 to 1000000")
   (colorize (it "How?") RedColor))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (show-steps sep defs . l)
    (let ([-> (alg-code "->")])
      (let loop ([l l])
        (let ([step (list
                     (page-para
                      (vl-append
                       sep
                       defs
                       (car l)
                       ->
                       defs
                       (cadr l))))])
          (if (null? (cddr l))
              (list step)
              (cons step (loop (cdr l))))))))
  
  (slide/title/tall
   "Recursion in Scheme"
   'alts
   (show-steps
    font-size
    (mk-scheme-func "!")
    (prog "(f 1000000 0)")
    (prog* "(if (zero? 1000000)"
           "    0"
           "    (f (- 1000000 1) (+ 1000000 0))))")
    (prog "(f (- 1000000 1) (+ 1000000 0))")
    (prog "(f 999999 1000000)")
    (prog* "(if (zero? 999999)"
           "    1000000"
           "    (f (- 999999 1) (+ 999999 1000000))))")
    (prog "(f (- 999999 1) (+ 999999 1000000))")
    (prog "(f 999998 1999999)")
    (prog "...")
    (prog* "(if (zero? 0)"
           "    500000500000"
           "    (f (- 0 1) (+ 0 500000500000))))")
    (prog "500000500000")))
  
  (slide/title
   "Recursion in Scheme"
   (page-item "Our definition of Scheme says nothing about a" (it "stack"))
   (page-item "Each step from a small program produces a small program...")
   (page-item "We can forget the old small program after each step...")
   (page-item "So there's no reason to run out of anything!")
   'next
   (blank)
   (page-item "Does that mean that we can write anything, without worrying"
              "about running out of space?"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (mk-scheme-func2 pat)
    (prog$*
     pat
     "(define (f2 n)"
     "  (if (zero? n)"
     "      0"
     "      (+ n (f2 (- n 1)))))"))
  
  (slide/title
   "Recursion in Scheme"
   (page-para "What does the following program compute?")
   (blank)
   (vl-append
    font-size
    (mk-scheme-func2 "!")
    (prog*
     "(f2 1000000)"))
   'next
   (blank)
   (page-para (colorize (t "Answer:") BlueColor)
              "out of memory, maybe"))
  
  (slide/title/tall
   "Recursion in Scheme"
   'alts
   (show-steps
    (/ font-size 2)
    (mk-scheme-func2 "!")
    (prog "(f2 1000000)")
    (prog* "(if (zero? 1000000)"
           "    0"
           "    (+ 1000000 (f2 (- 1000000 1))))")
    (prog "(+ 1000000 (f2 (- 1000000 1)))")
    (prog "(+ 1000000 (f2 999999))")
    (prog* "(+ 1000000"
           "   (if (zero? 999999)"
           "       0"
           "       (+ 999999 (f2 (- 999999 1)))))")
    (prog* "(+ 1000000"
           "   (+ 999999 (f2 (- 999999 1))))")
    (prog* "(+ 1000000"
           "   (+ 999999 (f2 999998)))")
    (prog "...")
    (prog* "(+ 1000000"
           "   (+ 999999"
           "      (+ 999998 ... (+ 1 0))))")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "The Cost of Recursion"
   (page-item "Computing" 
              (hbl-append (prog "(f ") (it "n") (prog ")"))
              "takes"
              "O(1)"
              "space")
   (page-item "Computing" 
              (hbl-append (prog "(f2 ") (it "n") (prog ")"))
              "takes"
              (hbl-append (t "O(") (it "n") (t ")"))
              "space")
   'next
   (blank)
   (page-item "In Scheme, we write loops and more general forms of"
              "recursion in the same way, but there's still a difference in costs")
   (page-item "How does a Scheme programmer write a loop?"))
  
  (slide/title
   "Loops in Scheme"
   (page-item "A function (or set of functions) acts like a loop if every"
              "self-call is a" (dt "tail call"))
   (page-item "A tail call is a function call whose value is"
              "the final result for the caller")
   'alts
   (list (list (mk-scheme-func ".f .- n 1. .. n v..")
               (colorize (bit "a tail call") PurpleColor))
         
         (list (mk-scheme-func2 ".f2 .- n 1..")
               (colorize (bit "not a tail call") PurpleColor))
         (list (prog$*
                ".((odd)|(even)) .- n 1.."
                "(define (odd n)"
                "  (if (zero? n)"
                "      #f"
                "      (even (- n 1)))"
                "(define (even n)"
                "  (if (zero? n)"
                "      #t"
                "      (odd (- n 1)))")
               (colorize (bit "tail calls") PurpleColor))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Implementing Control Explicitly"
   (page-item "Ok, so how is it done?")
   'next
   (blank)
   (page-item "We'll change our interpreter to make control explicit")
   (page-item "Let's first see what a trace of evaluation should look like"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Evaluation"
   (blank)
   (alg-code "1")
   'next
   (eval-step*
    "1"
    "{}")
   'next
   (blank)
   (bt "Done!"))
  
  (slide/title
   "Evaluation"
   (blank)
   (alg-code "+(1, 2)")
   'next
   (eval-step*
    "+(1, 2)"
    "{}")
   'next
   (eval-step*
    "1"
    "{}")
   'next
   'alts
   (list (list (bt "Done?"))
         (list
          (eval-step*
           "2"
           "{}")))
   'next
   (bt "How do we know when we're done?")
   (bt "How do we know what's left to do?"))
  
  (slide/title
   "Evaluation with To-Do List"
   (alg-code "1")
   'next
   (eval-step
    "1"
    "{}"
    "[done]")
   'alts
   (list (list
          (page-item "Keep a to-do list, passed to evaluator"))
         (list
          (kont-step
           "1"
           "[done]")))
   'alts
   (list (list
          (page-item "When we get a value, go into to-do-checking mode"))
         (list
          (blank)
          (bt "Done!"))))
    
  (slide/title
   "Evaluation with To-Do List"
   (alg-code "+(1, 2)")
   'next
   (eval-step
    "+(1, 2)"
    "{}"
    "[done]")
   'next
   (eval-step
    "1"
    "{}"
    "[addexp 2 in {} then [done]]")
   'alts
   (list (list 
          (page-item "When evaluating sub-expressions, extend the to-do list")
          (vc-append
           line-sep
           (page-item (alg-code "addexp") "is an abbreviation for:")
           (vl-append line-sep
                      (it "remember the result, evaluate another expression,")
                      (it "then add the two results"))))
         (list
          (kont-step
           "1"
           "[addexp 2 in {} then [done]]"))))
  
  (slide/title
   "Evaluation with To-Do List"
   (kont-step
    "1"
    "[addexp 2 in {} then [done]]")
   'next
   (eval-step
    "2"
    "{}"
    "[addval 1 then [done]]")
   'alts
   (list (list
          (page-item "To do" (alg-code "addexp") ", we start"
                     "evaluating the remembered expression"
                     "in the remembered environment")
          (page-item "Extend to-do list to remember the value we already"
                     "have, and remember to do an addition later")
          (vc-append
           line-sep
           (page-item (alg-code "addval") "is an abbreviation for:")
           (it "add the result with a remembered result")))
         (list
          (kont-step
           "2"
           "[addval 1 then [done]]")))
   'next
   (kont-step
    "3"
    "[done]")
   'next
   (blank)
   (bt "Done!"))
  
  (slide/title
   "Evaluation with To-Do List"
   (blank)
   (alg-code "+(1, +(2, 3))")
   (blank)
   'alts
   (steps
    (eval-step "+(1, +(2, 3))" "{}" "[done]")
    (eval-step "1" "{}" "[addexp +(2, 3) in {} then [done]]")
    (kont-step "1" "[addexp +(2, 3) in {} then [done]]")
    (eval-step "+(2, 3)" "{}" "[addval 1 then [done]]")
    (eval-step "2" "{}" "[addexp 3 in {}" "then [addval 1 then [done]]]")
    (kont-step "2" "[addexp 3 in {}" "then [addval 1 then [done]]]")
    (eval-step "3" "{}" "[addval 2 then [addval 1 then [done]]]")
    (kont-step "3" "[addval 2 then [addval 1 then [done]]]")
    (kont-step "5" "[addval 1 then [done]]")
    (kont-step "6" "[done]")))
  
  (slide/title
   "Evaluation with To-Do List"
   (blank)
   (alg-code* 
    "let f = proc(y)y"
    "   in (f 10)")
   (blank)
   'alts
   (steps
    (eval-step "let f = proc(y)y in (f 10)" "{}" "[done]")
    (eval-step "proc(y)y" "{}" "[let f in (f 10) {} then [done]]")
    (kont-step "<y,y,{}>" "[let f in (f 10) {} then [done]]")
    (eval-step "(f 10)" "{f=<y,y,{}>}" "[done]")
    (eval-step "f" "{f=<y,y,{}>}" "[apparg 10 in {f=<y,y,{}>}" "then [done]]")
    (kont-step "<y,y,{}>" "[apparg 10 in {f=<y,y,{}>}" "then [done]]")
    (eval-step "10" "{f=<y,y,{}>}" "[app <y,y,{}> then [done]]")
    (kont-step "10" "[app <y,y,{}> then [done]]")
    (eval-step "y" "{y=10}" "[done]")
    (kont-step "10" "[done]")))
  
  (set-wide! #f)
  
  (slide/title
   "To-Do Lists"
   (page-item "To-do list is called the" (bit "continuation"))
   (page-item "It makes the Scheme context in our interpreter explicit")
   'next
   (page-para "The interpreter now consists of two main functions:")
   (page-item (tt "eval-expression : expr env cont -> expval"))
   (eval-step "1" "{}" "[done]")
   (page-item (tt "apply-cont : value cont -> expval"))
   (kont-step "1" "[done]"))
  
  (define (xform w i)
    (page-para
     (vl-append
      line-sep
      w
      (tt "=")
      i)))
  
  (slide/title
   "Continuation Datatype"
   (blank)
   (prog*
    "(define-datatype"
    " continuation continuation?"
    " (done-cont)"
    " (app-arg-cont (rand expression?)"
    "               (env environment?)"
    "               (cont continuation?))"
    " (app-cont (rator value?)"
    "           (cont continuation?))"
    " ...)"))
  
  (slide/title
   "Continuation Datatype"
   (xform (alg-code "[done]") (tt "(done-cont)"))
   'next
   (blank)
   (xform (alg-code "[addval 1 then [done]]") 
          (tt "(prim-cont (add-prim) 1 (done-cont))"))
   'next
   (blank)
   (xform (alg-code "[addexp y in {y=10} then [done]]") 
          (prog*
           "(prim-other-cont"
           "  (add-prim)  (var-exp 'y) "
           "  (extend-env '(y) '(10) (empty-env))"
           "  (done-cont))")))
  
  (slide/title
   "Continuation Datatype"
   (blank)
   (xform (alg-code "[let f in (f 10) {} then [done]]") 
          (prog*
           "(let-cont 'f (app-exp (var-exp 'f) "
           "                      (list-exp 10))"
           "          (empty-env)"
           "          (done-cont))")))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Interpreter"
   (blank)
   (prog*
    "(define eval-program "
    "  (lambda (pgm)"
    "   (cases program pgm"
    "     (a-program (body)"
    "        (eval-expression body"
    "                         (init-env) "
    "                         (done-cont))))))"))
  
  (slide/title
   "Interpreter"
   (prog*
    "(define (eval-expression exp env cont)"
    "  (cases expression exp"
    "    (lit-exp (datum) "
    "      (apply-cont cont datum))"
    "    (var-exp (id) "
    "      (apply-cont cont (apply-env env id)))"
    "    (proc-exp (id body-exp)"
    "      (apply-cont cont"
    "                  (closure id body-exp env)))"
    "    ...)))"
    " "
    "(define (apply-cont cont val)"
    "  (cases continuation cont"
    "    (done-cont () val) "
    "    ...))"))
  
  (slide/title
   "Interpreter: Let"
   (blank)
   (prog$*
    ";.*"
    "... ; in eval-expression:"
    "(let-exp (id exp body-exp) "
    "   (eval-expression"
    "    exp env"
    "    (let-cont id body-exp env cont)))"
    "..."
    "... ; in apply-cont:"
    "(let-cont (id body env cont)"
    "   (eval-expression"
    "    body (extend-env (list id) (list val)"
    "                     env)"
    "    cont))"
    "..."))
  
  (slide/title
   "Interpreter: Primitives"
   (prog$*
    ";.*"
    "... ; in eval-expression:"
    "(primapp-exp (prim rand1 rand2)"
    "   (eval-expression"
    "    rand1 env"
    "    (prim-other-cont prim rand2 env cont)))"
    "..."
    "... ; in apply-cont:"
    "(prim-other-cont (prim arg2 env cont)"
    "   (eval-expression"
    "    arg2 env"
    "    (prim-cont prim val cont)))"
    "(prim-cont (prim arg1-val cont)"
    "   (apply-cont "
    "    cont "
    "    (apply-primitive prim arg1-val val)))"
    "..."))
  
  (slide/title
   "Interpreter: Application"
   (prog$*
    ";.*"
    "... ; in eval-expression:"
    "(app-exp (rator rand)"
    "  (eval-expression "
    "   rator env"
    "   (app-arg-cont rand env cont)))"
    "..."
    "...  ; in apply-cont:"
    "(app-arg-cont (rand env cont)"
    "   (eval-expression rand env"
    "                    (app-cont val cont)))"
    " (app-cont (f cont)"
    "   (apply-proc f val cont))"
    "..."))
  
  (slide/title
   "Interpreter: If"
   (blank)
   (prog$*
    ";.*"
    "... ; in eval-expression:"
    "(if-exp (test then else)"
    "   (eval-expression "
    "    test env"
    "    (if-cont then else env cont)))"
    "..."
    "...  ; in apply-cont:"
    "(if-cont (then else env cont)"
    "   (eval-expression"
    "    (if (zero? val) else then)"
    "    env cont))"
    "..."))
  
    (slide/title
     "Interpreter: Complete Implementation"
     (blank)
     (page-para* "(in DrScheme, instrumented to show traces)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Variable and Control Stacks"
   (page-para "One more way to look at continuations:")
   (page-item "environment = variable stack")
   (page-item "continuation = control stack")
   (blank)
   (page-para "In most imperative languages (e.g., C, Java), the"
              "variable stack and control stack are merged")
   (page-para "That's why a special form is needed for loops"))
  
  'done)
