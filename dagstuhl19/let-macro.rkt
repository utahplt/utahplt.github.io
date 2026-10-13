#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (if0E [tst : Exp]
        [thn : Exp]
        [els : Exp])
  (quoteE [s-exp : S-Exp])
  (consE [fst : Exp]
         [rst : Exp])
  (firstE [lst : Exp])
  (restE [lst : Exp])
  (empty?E [arg : Exp]))

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (symV [s : Symbol])
  (listV [l : (Listof Value)]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse* [s : S-Exp] [env : Env]) : Exp
  (cond
    [(s-exp-match? `{let-macro {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (let ([proc (interp (parse (second bs))
                           mt-env)])
         (parse* (third (s-exp->list s))
                 (extend-env (bind (s-exp->symbol (first bs))
                                   proc)
                             env))))]
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse* (second (s-exp->list s)) env)
            (parse* (third (s-exp->list s)) env))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse* (second (s-exp->list s)) env)
            (parse* (third (s-exp->list s)) env))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse* (third (s-exp->list s)) env))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse* (second (s-exp->list s)) env)
           (parse* (third (s-exp->list s)) env)
           (parse* (fourth (s-exp->list s)) env))]
    [(s-exp-match? `{quote ANY} s)
     (quoteE (second (s-exp->list s)))]
    [(s-exp-match? `{cons ANY ANY} s)
     (consE (parse* (second (s-exp->list s)) env)
            (parse* (third (s-exp->list s)) env))]
    [(s-exp-match? `{first ANY} s)
     (firstE (parse* (second (s-exp->list s)) env))]
    [(s-exp-match? `{rest ANY} s)
     (restE (parse* (second (s-exp->list s))env))]
    [(s-exp-match? `{empty? ANY} s)
     (empty?E (parse* (second (s-exp->list s)) env))]
    [(s-exp-match? `{ANY ANY ...} s)
     (let ([rator (first (s-exp->list s))])
       ;; try as macro...
       (try
        (parse* (apply-macro (lookup (s-exp->symbol rator) env)
                             s)
                env)
        ;; ... fall back to application
        (lambda ()
          (if (= (length (s-exp->list s)) 2)
              (appE (parse* rator env)
                    (parse* (second (s-exp->list s)) env))
              (error 'parse "invalid input")))))]
    [else (error 'parse "invalid input")]))

(define (parse [s : S-Exp]) : Exp
  (parse* s mt-env))

(module+ test
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{let-macro {[m {lambda {s} {quote {* 1 2}}}]}
                  {+ {m} 3}})
        (plusE (multE (numE 1) (numE 2))
               (numE 3))))

;; apply-macro ----------------------------------------
(define (apply-macro [transformer : Value] [s : S-Exp]) : S-Exp
  (type-case Value transformer
    [(closV arg body env)
     (value->s-exp
      (interp body (extend-env
                    (bind arg (s-exp->value s))
                    env)))]
    [else (error 'apply-macro "not a function")]))

(module+ test
  (test (apply-macro (closV 'a (consE (idE 'a) (quoteE `{})) mt-env)
                     `{1 2})
        `{{1 2}})

  (test/exn (apply-macro (numV 1) `{1 2})
            "not a function"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(lamE n body)
     (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
    [(if0E tst thn els) (type-case Value (interp tst env)
                          [(numV n) (if (zero? n)
                                        (interp thn env)
                                        (interp els env))]
                          [else (error 'interp "not a number")])]
    [(quoteE s-exp) (s-exp->value s-exp)]
    [(consE fst rst)
     (local [(define fst-v (interp fst env))
             (define rst-v (interp rst env))]
       (type-case Value rst-v
         [(listV l) (listV (cons fst-v l))]
         [else (error 'interp "not a list")]))]
    [(firstE lst) (type-case Value (interp lst env)
                    [(listV l) (if (empty? l)
                                   (error 'interp "empty list")
                                   (first l))]
                    [else (error 'interp "not a list")])]
    [(restE lst) (type-case Value (interp lst env)
                   [(listV l) (if (empty? l)
                                  (error 'interp "empty list")
                                  (listV (rest l)))]
                   [else (error 'interp "not a list")])]
    [(empty?E lst) (type-case Value (interp lst env)
                     [(listV l) (numV (if (empty? l) 0 1))]
                     [else (numV 1)])]))


;; value<->s-exp ----------------------------------------

(define (value->s-exp [v : Value]) : S-Exp
  (type-case Value v
    [(numV n) (number->s-exp n)]
    [(symV s) (symbol->s-exp s)]
    [(listV l) (list->s-exp (map value->s-exp l))]
    [else (error 'value->s-exp "cannot convert")]))

(define (s-exp->value [s : S-Exp])
  (cond
    [(s-exp-number? s) (numV (s-exp->number s))]
    [(s-exp-symbol? s) (symV (s-exp->symbol s))]
    [(s-exp-list? s) (listV (map s-exp->value
                                 (s-exp->list s)))]
    [else (error 's-exp->value "cannot convert")]))

;; helpers ----------------------------------------

(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value])
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(define (lookup [n : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "free variable")]
    [else (cond
            [(symbol=? n (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup n (rest env))])]))

;; fib test ----------------------------------------

(module+ test
  (test (interp
         (parse
          ;; Start by defining the `let` macro:
          `{let-macro {[let {lambda {s}
                              {cons {cons 'lambda
                                          {cons
                                           {cons {first {first {first {rest s}}}}
                                                 '{}}
                                           {cons {first {rest {rest s}}}
                                                 '{}}}}
                                    {cons {first {rest {first {first {rest s}}}}}
                                          '{}}}}]}
                      ;; Use `let` the same as if it were built in:
                      {let {[Y {lambda {f}
                                 {{lambda {fX}
                                    {fX fX}}
                                  {lambda {fX}
                                    {f {lambda {x} {{fX fX} x}}}}}}]}
                        {let {[fib
                               {Y {lambda {fib}
                                    ;; Fib:
                                    {lambda {n}
                                      {if0 n
                                           1
                                           {if0 {+ n -1}
                                                1
                                                {+ {fib {+ n -1}}
                                                   {fib {+ n -2}}}}}}}}]}
                          {fib 15}}}})
         mt-env)
        (numV 987)))
