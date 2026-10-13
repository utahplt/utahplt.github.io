#lang racket
(require slideshow/pict
         slideshow/code-pict
         racket/draw
         "in-file.rkt"
         "pict-style.rkt")

(provide lists-slide
         eval-when-slide
         phase-slide
         template-slide
         begin-for-syntax-slide)

(define (t str)
  (text str 'roman ((get-current-code-font-size))))
(define (it str)
  (text str '(italic . roman) ((get-current-code-font-size))))
(define (tt str)
  ((current-code-tt) str))

(define gap-size 10)

(define-syntax-rule (scode e ...)
  (scale (code e ...) 0.8))

(define a-size (/ gap-size 2))
(define l-width 1)

(define lispy-suffix "lsp")

(define (nt s) (colorize (t s) "blue"))

(define (link p from to
              #:from [from-find cb-find]
              #:to [to-find c*t-find]
              #:color [color runtime-color]
              #:alpha [alpha 1.0]
              #:hilite? [hilite? #f]
              #:start-angle [start-angle #f]
              #:end-angle [end-angle #f])
  (let ([p (if hilite?
               (pin-arrow-line (+ a-size 4)
                               p
                               from from-find 
                               to (adj to-find -2 2)
                               #:color "pink"
                               #:line-width 7)
               p)])
    (pin-arrow-line a-size
                    p
                    from from-find 
                    to to-find
                    #:color color
                    #:alpha alpha
                    #:line-width l-width
                    #:start-angle start-angle
                    #:end-angle end-angle)))

(define (llink p from to
               #:from [from-find rc-find]
               #:to [to-find c*t-find]
               #:alpha [alpha 1.0])
  (link p from to
        #:from from-find
        #:to to-find
        #:alpha alpha
        #:color load-color))

(define (lists-slide #:hilite-use? [hilite-use? #f]
                     #:show-err? [show-err? #f]
                     #:show-loads? [show-loads? #t]
                     #:show-deps? [show-deps? #t]
                     #:bad-order? [bad-order? #f]
                     #:show-rev? [show-rev? #f]
                     #:hilite-bad? [hilite-bad? #f]
                     #:add-load? [add-load? #f]
                     #:use? [use? #f]
                     #:use-note? [use-note? use?]
                     #:show-flat? [show-flat? (not use?)])

  (define (sel-use l u)
    (lbl-superimpose 
     ((if use? ghost values) l)
     ((if use? values ghost) u)))

  (define load-grocery (sel-use
                        (code (load "grocery.scm"))
                        (code (use "grocery.lsp"))))
  (define load-list-load (code (load "list.scm")))
  (define load-list-use (code (use "list.lsp")))
  (define load-list (sel-use load-list-load
                             load-list-use))
  (define load-top-10-load (code (load "top-10.scm")))
  (define load-top-10-use (code (use "top-10.lsp")))
  (define load-top-10 (sel-use load-top-10-load
                               load-top-10-use))
  (define load-list-too-load (code (load "list.scm")))
  (define load-list-too-use (code (use "list.lsp")))
  (define load-list-too (sel-use load-list-too-load
                                 load-list-too-use))

  (define fold-use ((if hilite-use? hilite values)
                    (code fold)))
  (define err-hilite (if show-err?
                         (lambda (p) (hilite p #:color "pink"))
                         values))

  (define main-body
    (code
     .... shop ....
     .... count-down ....))
  (define grocery-body
    (code
     (define (shop l)
       .... #,fold-use ....)))
  (define top-10-body
    (code
     (define (count-down l)
       .... #,(err-hilite fold-use) ....)))
  (define list-body
    (code
     (define fold ....)))

  (define sfx (if use? lispy-suffix "scm"))

  (define magazine
    (mk-file #:name "main"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,(if bad-order? load-top-10 load-grocery)
                    #,(if bad-order? load-grocery load-top-10)
                    #,main-body)))

  (define grocery
    (mk-file #:name "grocery"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,load-list
                    #,grocery-body)))
  
  (define top-10
    (let ([p
           (mk-file #:name "top-10"
                    #:suffix sfx
                    #:shape (if use? 'semi-mod 'file)
                    (scode #,((if add-load? values ghost) load-list-too)
                           #,top-10-body))])
      (if show-err?
          (refocus (vr-append (/ gap-size 3)
                              p
                              (colorize (if use?
                                            (it "fold: no binding")
                                            (it "fold: undefined"))
                                        "red"))
                   p)
          p)))
  
  (define lists
    (mk-file #:name "list"
             #:suffix sfx
             #:shape (if use? 'semi-mod 'file)
             (scode #,list-body)))

  (define link-a (if hilite-bad? 0.3 1.0))
  (define load-a 1.0)

  (let* ([p (vc-append
             (* 1 gap-size)
             (if show-rev?
                 (pin-arrows-line (* a-size 1.5)
                                  magazine
                                  load-grocery (adj lc-find -3 0)
                                  load-top-10 (adj lc-find -3 0)
                                  #:color "red"
                                  #:line-width 7
                                  #:start-angle (- pi)
                                  #:start-pull 1
                                  #:end-angle 0
                                  #:end-pull 1)
                 magazine)
             (hc-append
              (* 3 gap-size)
              grocery
              top-10)
             lists)]
         [p (if show-deps?
                (let* ([p (link p magazine grocery #:from (adj cb-find (- gap-size) 0) #:alpha link-a)]
                       [p (link p magazine top-10 #:from (adj cb-find gap-size 0) #:to ct*-find #:alpha link-a)]
                       [p (link p grocery lists #:to lt*-find #:alpha link-a)]
                       [p (link p top-10 lists #:to rt-find #:hilite? hilite-bad?)])
                  p)
                p)]
         [p (if show-loads?
                (let* ([p (llink p load-grocery grocery #:to ct-find #:from lc-find #:alpha load-a)]
                       [p (llink p (if use? load-top-10-use load-top-10) top-10 #:alpha load-a)]
                       [p (llink p (if use? load-list-use load-list) lists #:to ct*-find #:alpha load-a)]
                       [p (if add-load?
                              (llink p load-list-too lists #:from cb-find #:alpha load-a)
                              p)])
                  p)
                p)]
         [p (rt-superimpose
             (rt-superimpose
              (inset p 0 0 (* 7 gap-size) 0)
              ((if show-flat? values ghost)
               (scale
                (hc-append
                 (tt "= ")
                 (mk-file #:shape 'file
                          #:color (make-color 200 230 200)
                          (scode
                           #,((if add-load? values ghost) list-body)
                           #,(if bad-order? top-10-body list-body)
                           #,(if bad-order? list-body grocery-body)
                           #,(if bad-order? grocery-body top-10-body)
                           #,main-body)))
                0.75))))])
    (scale p paper-scale)))

#;
(define (use-slides)
  (lists-slide #:show-loads? #f #:show-deps? #f #:show-flat? #f)
  (lists-slide #:show-deps? #f #:show-flat? #f)
  (lists-slide #:show-deps? #f)
  (lists-slide #:hilite-use? #t #:show-deps? #f)
  (lists-slide #:hilite-use? #t)
  (lists-slide #:bad-order? #t #:show-rev? #t #:show-err? #t)
  (lists-slide #:bad-order? #t #:add-load? #t)
  (lists-slide #:add-load? #t #:use? #t #:use-note? #f)
  (lists-slide #:add-load? #t #:use? #t)
  (lists-slide #:add-load? #f #:show-err? #t #:use? #t))


(define (eval-when-slide #:gui? [gui? #f]
                         #:eval-when? [eval-when? #f]
                         #:macro? [macro? eval-when?]
                         #:use-means-load? [use-means-load? #f]
                         #:use-means-compile? [use-means-compile? #f]
                         #:unhappy? [unhappy? #f]
                         #:require? [require? #f]
                         #:compile? [compile? (not require?)]
                         #:decl-note? [decl-note? #f]
                         #:require-note? [require-note? #f]
                         #:horiz? [horiz? #t])
  (define a-require (code require))
  (define use-grocery (if require?
                          (code (#,a-require "grocery.rkt"))
                          (code (use "grocery.lsp"))))
  (define use-gui (if require?
                      (code (require "gui.rkt"))
                      (code (use "gui.lsp"))))
  (define groceries-id (code groceries))
  (define init-gui-application!-code 
    (code init-gui-application!))
  (define kitchen-body
    (scode
     #,(cond
        [require?
         (code #,use-grocery
               code:blank)]
        [eval-when?
         ;; hack: want `define'-style indentation:
         (let-syntax ([define (make-code-transformer #'(code eval-when))])
           (code (define (load compile)
                   #,use-grocery)))]
        [else
         (code #,use-grocery
               code:blank)])
     #,(ltl-superimpose
        ((if macro? values ghost)
         (code (shop (#,groceries-id bread
                                     [2 milk] 
                                     apples))))
        ((if macro? ghost values)
         (code (shop ....))))))
  (define grocery-body
    (scode #,((if gui? values ghost)
              (code #,use-gui))
           #,((if macro? values ghost)
              (code
               (define-syntax groceries ....)))
           (define shop ...)
           #,((if gui? values ghost)
              (code
               (define list-editor-gui ....)))))
  (define gui-body
    (scode (#,init-gui-application!-code)
           ....))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width kitchen-body)
                   (pict-width grocery-body)
                   (pict-width gui-body))
              (pict-width p))
           0))
  (define shape (if require? 'mod 'semi-mod))
  (define sfx (if require? "rkt" lispy-suffix))
  (define grocery
    (mk-file #:name "grocery"
             #:suffix sfx
             #:shape shape
             (std-size grocery-body)))
  (define gui
    (mk-file #:name "gui"
             #:suffix sfx
             #:shape shape
             (std-size gui-body)))
  (let* ([pre-gui ((if horiz? ht-append vc-append)
                   gap-size
                   (mk-file #:name "kitchen"
                            #:suffix sfx
                            #:shape shape
                            (std-size kitchen-body))
                   grocery)]
         [p ((if horiz? vr-append vc-append)
             (if horiz? 0 (* 1 gap-size))
             pre-gui
             ((if gui? values ghost)
              gui))]
         [p (link p use-grocery grocery
                  #:color (if require? runtime-color load-color)
                  #:from (if horiz? rc-find lc-find)
                  #:to (if horiz?
                           (lambda (p sp)
                             (define-values (x y) (lt*-find p sp))
                             (define-values (x0 y0) (rc-find p use-grocery))
                             (values x y0))
                           lt*-find)
                  #:start-angle (if horiz? #f (- pi))
                  #:end-angle (if horiz? #f (* pi -1/6)))]
         [p (if gui?
                (link p use-gui gui
                      #:color (if require? runtime-color load-color)
                      #:from (if horiz? cb-find lc-find)
                      #:to (if horiz? ct*-find lt*-find)
                      #:start-angle (if horiz? #f (- pi))
                      #:end-angle (if horiz? #f (* pi -1/6)))
                p)])
    (scale (inset (if gui?
                      p
                      (refocus p pre-gui))
                  (* 2 gap-size) ; to cover arrow arcs
                  0)
           paper-scale)))

#;
(define (eval-when-slides)
  (eval-when-slide #:compile? #f)
  (eval-when-slide #:macro? #t #:compile? #f)
  (eval-when-slide #:macro? #t)
  (eval-when-slide #:macro? #t #:use-means-load? #t)
  (eval-when-slide #:macro? #t #:use-means-load? #t #:unhappy? #t)
  (eval-when-slide #:gui? #t)
  (eval-when-slide #:gui? #t #:use-means-compile? #t)
  (eval-when-slide #:gui? #t #:use-means-compile? #t #:unhappy? #t)
  (eval-when-slide #:gui? #t #:require? #t)
  #; (eval-when-slide #:gui? #t #:require? #t #:decl-note? #t)
  (eval-when-slide #:gui? #t #:require? #t #:require-note? #t)
  #; (eval-when-slide #:gui? #t #:require? #t))

(define (phase-slide #:run-time? [run-time? #t]
                     #:run-time-arrows? [run-time-arrows? run-time?]
                     #:compile-time? [compile-time? #t]
                     #:compile-time-arrows? [compile-time-arrows? compile-time?]
                     #:template? [template? #f]
                     #:split? [split? #f]
                     #:both? [both? split?]
                     #:kitchen? [kitchen? #t])

  (define (r-encloud #:dh [dh 0] p)
    (if run-time?
        (encloud #:dh dh p)
        p))
  (define (c-encloud #:dh [dh 0] p)
    (if compile-time?
        (encloud #:dh dh
                 #:color comptime-color
                 p) 
        p))

  (define grocery-ref (code "grocery.rkt"))
  (define list-ref (code "list.rkt"))
  (define list-ref2 (launder list-ref))
  (define list-stx-ref (code (for-syntax "list.rkt")))

  (define kitchen-body
    (scode
     (require #,list-ref
              #,grocery-ref)
     (define weekly #,(r-encloud (code (groceries ....))))
     #,(r-encloud (code (shop .... fold ....)))))
  (define fold (hbl-append ((if template? values ghost) (tt "#'"))
                           (let ([f (code fold)])
                             (if template?
                                 (r-encloud #:dh -4 f)
                                 f))))
  (define grocery-body
    (scode
     (require "gui.rkt"
              #,(if template?
                    list-ref2
                    (if both?
                        (code #,list-ref2 #,list-stx-ref)
                        (code #,list-stx-ref))))
     (define-syntax groceries #,(c-encloud (code .... #,fold ....))) #,(ghost (code 1))
     (define shop #,(r-encloud (if (or template? both?)
                                   (code .... fold ....)
                                   (code ....))))
     (define list-editor-gui #,(r-encloud (code ....)))))
  (define list-body
    (scode
     (define fold #,(encloud (code ....)))))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width kitchen-body)
                   (pict-width grocery-body)
                   (pict-width list-body))
              (pict-width p))
           0))

  (define spacer (blank gap-size 0))

  (define kitchen-mod
    (mk-file #:name "kitchen"
             #:suffix "rkt"
             (std-size kitchen-body)))
  (define grocery-mod
    (mk-file #:name "grocery"
             #:suffix "rkt"
             (std-size grocery-body)))
  (define core-list-mod
    (mk-file #:name "list"
             #:suffix "rkt"
             (std-size list-body)))
  (define list-mod2 ((if split? values ghost) (launder core-list-mod)))
  (define list-mod
    (refocus (ct-superimpose
              (inset list-mod2 gap-size gap-size 0 0)
              core-list-mod)
             core-list-mod))

  (define key
    (hbl-append
     gap-size
     ((if run-time? values ghost)
      (hbl-append (encloud (tt " ")) (t "  = run time")))
     ((if compile-time? values ghost)
      (hbl-append
       (encloud (tt " ") #:color comptime-color) (t "  = compile time")))))

  (let* ([post-kitchen (vc-append
                        gap-size
                        grocery-mod
                        list-mod)]
         [p (vc-append gap-size
                       (blank)
                       ((if kitchen? values ghost) kitchen-mod)
                       post-kitchen)]
         [a (/ gap-size 2)]
         [lw l-width]
         [p (if (and run-time-arrows?
                     kitchen?)
                (pin-arrow-line a p
                                list-ref lc-find
                                list-mod lt*-find
                                #:start-angle (* pi -9/10)
                                #:start-pull 0.75
                                #:end-angle (* pi -1/6)
                                #:end-pull 1/2
                                #:line-width lw
                                #:color runtime-color)
                p)]
         [p (if (and run-time-arrows?
                     kitchen?)
                (pin-arrow-line a p
                                grocery-ref lc-find
                                grocery-mod lt*-find
                                #:start-angle (* pi -9/10)
                                #:start-pull 1
                                #:end-angle (* pi -1/6)
                                #:end-pull 1/2
                                #:line-width lw
                                #:color runtime-color)
                p)]
         [p (if (and (or template? both?)
                     run-time-arrows?)
                (pin-arrow-line a p
                                list-ref2 lc-find
                                list-mod lt*-find
                                #:start-angle (* pi -9/10)
                                #:start-pull 0.75
                                #:end-angle (* pi -1/6)
                                #:end-pull 1/2
                                #:line-width lw
                                #:color runtime-color)
                p)]
         [p (if (or template?
                    (not compile-time-arrows?))
                p
                (pin-arrow-line a p
                                list-stx-ref rc-find
                                (if split? list-mod2 list-mod) rt-find
                                #:start-angle 0
                                #:start-pull 1
                                #:end-angle (* pi -5/6)
                                #:end-pull 1/2
                                #:line-width lw
                                #:color comptime-color))]
         [p (if kitchen? 
                (lt-superimpose key p)
                p)])
    (scale
     (if kitchen?
         (inset p (* 6 gap-size) 0 (* 3 gap-size) 0)
         (inset (refocus p post-kitchen)
                (* 3 gap-size) 0
                (* 4 gap-size) (if split? gap-size 0)))
     paper-scale)))

#;
(define (phase-slides)
  (phase-slide #:run-time? #f #:compile-time? #f)
  (phase-slide #:run-time-arrows? #f #:compile-time? #f)
  (phase-slide #:compile-time? #f)
  (phase-slide #:compile-time-arrows? #f)
  (phase-slide)
  (phase-slide #:template? #t)
  (phase-slide #:both? #t)
  (phase-slide #:split? #t))

(define (template-slide)

  (define (r-encloud #:dh [dh 0] p)
    (encloud #:dh dh p))
  (define (c-encloud #:dh [dh 0] p)
    (encloud #:dh dh
             #:color comptime-color
             p))
  (define (t-encloud #:dh [dh 0] p)
    (encloud #:dh dh
             #:color tmpltime-color
             p))

  (define gen-list-code-ref (code (for-syntax "gen-list-code.rkt")))
  (define list-tmpl-ref (code (for-template "list.rkt")))

  (define grocery-body
    (scode
     (require #,gen-list-code-ref)
     (define-syntax groceries #,(c-encloud (code .... gen-code ....)))))
  (define gen-list-code-body
    (scode
     (require #,list-tmpl-ref)
     (define gen-code #,(encloud #:dh 2 (code ... #'#,(t-encloud (code fold)) ...)))))
  (define list-body
    (scode
     (define fold #,(encloud (code ....)))))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width grocery-body)
                   (pict-width gen-list-code-body)
                   (pict-width list-body))
              (pict-width p))
           0))

  (define spacer (blank gap-size 0))

  (define grocery-mod
    (mk-file #:name "grocery"
             #:suffix "rkt"
             (std-size grocery-body)))
  (define gen-list-code-mod
    (mk-file #:name "gen-list-code"
             #:suffix "rkt"
             (std-size gen-list-code-body)))
  (define list-mod
    (mk-file #:name "list"
             #:suffix "rkt"
             (std-size list-body)))

  (define key
    (hbl-append (t-encloud (tt " ")) (t "  = template time")))

  (let* ([p (vc-append
             gap-size
             grocery-mod
             gen-list-code-mod
             list-mod)]
         [a (/ gap-size 2)]
         [lw l-width]
         [p (pin-arrow-line a p
                            list-tmpl-ref lc-find
                            list-mod lt*-find
                            #:start-angle (* pi -9/10)
                            #:start-pull 0.75
                            #:end-angle (* pi -1/6)
                            #:end-pull 1/2
                            #:line-width lw
                            #:color tmpltime-color)]
         [p (pin-arrow-line a p
                            gen-list-code-ref lc-find
                            gen-list-code-mod lt*-find
                            #:start-angle (* pi -9/10)
                            #:start-pull 1
                            #:end-angle (* pi -1/6)
                            #:end-pull 1/2
                            #:line-width lw
                            #:color comptime-color)]
         [p (lt-superimpose key p)])
    (scale
     (inset p (* 3 gap-size) 0 0 0)
     paper-scale)))

(define (begin-for-syntax-slide)

  (define (r-encloud #:dh [dh 0] p)
    (encloud #:dh dh p))
  (define (c-encloud #:dh [dh 0] p)
    (encloud #:dh dh
             #:color comptime-color
             p))

  (define list-ref (code "list.rkt"))

  (define grocery-body
    (scode
     (require #,list-ref)
     (begin-for-syntax
      (define gen-code #,(c-encloud #:dh 2 (code ... #'#,(encloud (code fold)) ...))))
     (define-syntax groceries #,(c-encloud (code .... gen-code ....)))))
  (define list-body
    (scode
     (define fold #,(encloud (code ....)))))
  (define (std-size p)
    (inset p 0 0
           (- (max (pict-width grocery-body)
                   (pict-width list-body))
              (pict-width p))
           0))

  (define spacer (blank gap-size 0))

  (define grocery-mod
    (mk-file #:name "grocery"
             #:suffix "rkt"
             (std-size grocery-body)))
  (define list-mod
    (mk-file #:name "list"
             #:suffix "rkt"
             (std-size list-body)))

  (let* ([p (vc-append
             gap-size
             grocery-mod
             list-mod)]
         [a (/ gap-size 2)]
         [lw l-width]
         [p (pin-arrow-line a p
                            list-ref lc-find
                            list-mod lt*-find
                            #:start-angle (* pi -9/10)
                            #:start-pull 0.75
                            #:end-angle (* pi -1/6)
                            #:end-pull 1/2
                            #:line-width lw
                            #:color runtime-color)])
    (scale
     (inset p (* 3 gap-size) 0 0 0)
     paper-scale)))

(module+ main
  (require slideshow)
  (slide 
   (frame (scale (begin-for-syntax-slide) 2))))

