#lang racket
(require redex/reduction-semantics
         redex/pict
         slideshow/pict
         racket/draw)

(provide with-rewrites
         unscale
         ast)

(define model-scale 1)

(define (lit s) (text s (literal-style) (default-font-size)))
(define (trans s) (text s '(caps . roman) (default-font-size)))
(define (atag s) (trans s))
(define (ast s) (text s '(bold caps . roman) (default-font-size)))
(define (pt s) (text s 'roman (default-font-size)))

(define (between s a b)
  (build-lw s
            (+ (lw-line a) (lw-line-span a))
            0
            (+ (lw-column a) (lw-column-span a))
            (max 0 (- (lw-column b)
                      (+ (lw-column a) (lw-column-span a))))))

(define (re-lw new-e lw)
  (build-lw new-e
            (lw-line lw) (lw-line-span lw)
            (lw-column lw) (lw-column-span lw)))

(define (merge-lw new-e lw1 lw2)
  (build-lw new-e
            (lw-line lw1) (lw-line-span lw1)
            (lw-column lw1) (+ (lw-column-span lw2)
                               (- (lw-column lw2)
                                  (lw-column lw1)))))

(define (refit orig new)
  (append (list (between "" (just-before "" (car orig)) (car new)))
          new
          (list (between "" (just-after "" (last new)) (just-after "" (last orig))))))

(define (binary sep lws)
  (refit 
   lws
   (list
    (list-ref lws 2)
    (just-after sep (list-ref lws 2))
    (between "" (list-ref lws 2) (list-ref lws 3))
    (list-ref lws 3))))
 
(define (comma-ize lws #:has-closer? [has-closer? #t])
  (let loop ([lws lws])
    (cond
     [(or (null? (cdr lws))
          (and has-closer? (null? (cddr lws))))
      lws]
     [else
      (list*
       (car lws)
       (between ", " (car lws) (cadr lws))
       (loop (cdr lws)))])))

(define (constructor name)
  (lambda (lws)
    (refit lws
           (append
            (list (re-lw name (list-ref lws 1))
                  (between (pt "(") (list-ref lws 1) (list-ref lws 2)))
            (comma-ize (drop-right (cddr lws) 1) #:has-closer? #f)
            (list (re-lw (pt ")") (last lws)))))))
                  

(define (taller s) s)
#;
(define (taller s)
  (define p (text s (default-style) (default-font-size)))
  (define h (pict-height p))
  (drop-below-ascent (scale (launder p) 1 1.3) (* 0.1 h)))

(define (langle) (taller "\u2329"))
(define (rangle) (taller "\u232A"))

(define (angle-brackets who lws)
  (if (and who (= (length lws) 3))
      (list (re-lw (langle) (car lws))
            (re-lw (atag who) (cadr lws))
            (re-lw (rangle) (caddr lws)))
      (refit
       lws
       (let* ([open (re-lw (langle) (car lws))]
              [prev-tag (cadr lws)]
              [lws (drop (drop-right lws 1) 2)])
         (append (if who
                     (list open
                           (between "" open prev-tag)
                           (re-lw (atag who) prev-tag))
                     (list open (between "" open (car lws))))
                 (if who
                     lws
                     (comma-ize lws #:has-closer? #f))
                 (list (just-after (rangle) (last lws))))))))

(define (nt s) (text s (non-terminal-style) (default-font-size)))
(define (name) (nt "name"))
(define (mname) (nt "mname"))
(define (dname) (nt "dname"))

(define (make-subst-rewrite pre <- post)
  (lambda (lws)
    (refit
     lws
     (list (list-ref lws 2)
           (between pre
                    (list-ref lws 2)
                    (list-ref lws 3))
           (list-ref lws 3)
           (between (<-)
                    (list-ref lws 3)
                    (list-ref lws 4))
           (list-ref lws 4)
           (just-after post (list-ref lws 4))))))

(define (<-) (let-values ([(h w d a) (send (dc-for-text-size) get-text-extent "m")])
               (dc (lambda (dc x y)
                     (let ([p (send dc get-pen)])
                       (send dc set-pen (send p get-color) 0.5 'solid)
                       (let ([m (+ y (/ h 2))]
                             [x (+ 2 x)]
                             [w (- w 4)])
                         (send dc draw-line x m (+ x w) m)
                         (send dc draw-line x m (+ x (/ w 4)) (- m (/ w 4)))
                         (send dc draw-line x m (+ x (/ w 4)) (+ m (/ w 4))))
                       (send dc set-pen p)))
                   w h (- h d) d)))

(define extend-rewrite (make-subst-rewrite (pt "[") <- (pt "]")))
; (define extend-rewrite (make-subst-rewrite "+{" => "}"))

(define lookup-rewrite
  (lambda (lws) 
    (refit
     lws
     (append
      (list (list-ref lws 2) 
            (between (pt "(") (list-ref lws 2) (list-ref lws 3))
            (list-ref lws 3)
            (just-after (pt ")") (list-ref lws 3)))))))

(define (call-with-rewrites thunk)
  (parameterize ([literal-style (cons (make-color #x26 #x26 #x80) "Courier")]
                 [paren-style (cons (make-color #x84 #x3c #x24) "Courier")]
                 [default-font-size 10]
                 [metafunction-font-size 9]
                 [label-font-size 10]
                 [metafunction-pict-style 'left-right/compact-side-conditions]
                 [white-bracket-sizing (let ([orig (white-bracket-sizing)])
                                         (lambda (str size)
                                           (let-values ([(a b c d) (orig str size)])
                                             (define (less v [n 1]) (max (- v n) 0))
                                             (if (string=? str "[")
                                                 (values (less a 2) b c (less d))
                                                 (values a (less b 2) (less c) d)))))]
                 [where-make-prefix-pict (lambda ()
                                           ((current-text) " subject to " (default-style) (default-font-size)))]
                 [where-combine (lambda (l r)
                                  (define =-pict ((current-text) " = " (default-style) (default-font-size)))
                                  (hbl-append r =-pict l))])
    (with-atomic-rewriter*
     (lambda ()
       (with-compound-rewriter*
        (lambda ()
          (with-unquote-rewriter
           (lambda (lw)
             lw)
           (thunk)))
        (list
         'p+ (lambda (lws) (binary "+" lws))
         'p- (lambda (lws) (binary "\u2212" lws))
         'At (lambda (lws) (binary "@" lws))
         'Bind (lambda (lws) (angle-brackets #f lws))
         'RBind (lambda (lws) (angle-brackets #f lws))
         'Defn (lambda (lws) (angle-brackets #f (list* (car lws)
                                                       (cadr lws)
                                                       (let ([n (caddr lws)]
                                                             [p (cadddr lws)])
                                                         (merge-lw (list n
                                                                         (between "@" n p)
                                                                         p)
                                                                   n
                                                                   p))
                                                       (cddddr lws))))
         'Cell (lambda (lws) (angle-brackets #f lws))
         'Tup (lambda (lws) (angle-brackets #f lws))
         'Seq (lambda (lws)
                (if (= (length lws) 3)
                    (refit lws (list (re-lw "ε" (list-ref lws 0))))
                    (refit lws (drop-right (drop lws 2) 1))))
         'Sto (lambda (lws)
                (if (= (length lws) 3)
                    (refit lws (list (re-lw "∅" (list-ref lws 0))))
                    (refit
                     lws
                     (let ([lws (refit lws (drop-right (cddr lws) 1))])
                       (append (list (just-before (pt "{") (car lws)))
                               lws
                               (list (just-after (pt "}") (last lws))))))))
         'lookup lookup-rewrite
         'ready-lookup lookup-rewrite
         'mods-lookup lookup-rewrite
         'extend extend-rewrite
         'ready-extend extend-rewrite
         'mods-extend extend-rewrite
         'subst extend-rewrite
         'subst* extend-rewrite
         'Module (constructor (ast "Module"))
         'Stx (constructor (ast "Stx"))
         'Syntax (constructor (ast "Syntax"))
         'Quote (constructor (ast "Quote"))
         'Lambda (constructor (ast "Lambda"))
         'App (constructor (ast "App"))
         'Var (constructor (ast "Var"))
         'Ref (constructor (ast "Ref")))))
     (list
      'desc-name (lambda () (hbl-append (text "a token such as " 'roman (default-font-size))
                                        (lit "x")
                                        (text ", " 'roman (default-font-size))
                                        (lit "clock")
                                        (text ", or " 'roman (default-font-size))
                                        (lit "lambda")))
      'desc-mname (lambda () (text "a module name" 'roman (default-font-size)))
      'desc-dname (lambda () (text "a defined name" 'roman (default-font-size)))
      'desc-ast (lambda () (text "a compiled expression" 'roman (default-font-size)))
      'desc-expr (lambda () (text "an expression" 'roman (default-font-size)))
      'desc-M (lambda ()
                (hbl-append
                 (text "a mapping from " 'roman (default-font-size))
                 (nt "mname")
                 (text " to " 'roman (default-font-size))
                 (nt "mod")))
      'desc-Σ (lambda () (text "a store" 'roman (default-font-size)))
      'arb-expr (lambda () (text "whatever the macro allows" 'roman (default-font-size)))
      'nam name
      'mnam mname
      'dnam dname
      'Σ (lambda () (text "Σ" (default-style) (default-font-size)))
      'Unbound (lambda () (ast "Unbound"))
      'Ready (lambda () (ast "Ready"))
      'Value (lambda () (ast "Value"))
      'Macro (lambda () (ast "Macro"))))))
       
  
(define (with-atomic-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-atomic-rewriter
       (car l) (cadr l)
       (with-atomic-rewriter* thunk (cddr l)))))

(define (with-compound-rewriter* thunk l)
  (if (null? l)
      (thunk)
      (with-compound-rewriter
       (car l) (cadr l)
       (with-compound-rewriter* thunk (cddr l)))))

(define-syntax-rule (with-rewrites e)
  (call-with-rewrites (lambda () (scale e model-scale))))

(define (unscale p) (scale p (/ 1 model-scale)))