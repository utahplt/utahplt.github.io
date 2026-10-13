#lang racket
(require slideshow/pict
         slideshow/code-pict
         racket/draw)

(provide paper-scale
         runtime-color
         runtime-only-color
         comptime-color
         tmpltime-color
         load-color
         use-color
         new-color
         hilite
         encloud
         adj)

(define paper-scale 0.75)

(current-keyword-list (list* "define-objc-class"
                             "define-runtime-module-path-index"
                             "class"
                             "new"
                             "define-cstruct"
                             "use" "eval-when"
                             "groceries"
                             "defstx" "define-now-alias" 
                             "define-recently-alias" "define-about-then-alias"
                             (current-keyword-list)))

(current-code-font 'modern)

(define runtime-color (make-color 160 194 255)) ; lighter version of "dodgerblue"
(define runtime-only-color "deepskyblue")
(define comptime-color (make-color 170 132 240)) ; lighter version of "purple"
(define tmpltime-color "lime")
(define load-color "forestgreen")
(define use-color (make-color 200 255 200)) ; very light "lime"
(define new-color (make-color 255 185 140)) ; lighter version of "orange"

(define (hilite p 
                #:color [color use-color]
                #:style [style 'rectangle])
  (refocus (cc-superimpose
            (colorize ((if (eq? style 'ellipse)
                           filled-ellipse
                           filled-rectangle)
                       (pict-width p)
                       (pict-height p)) 
                      color)
            p)
           p))

(define (encloud #:color [color runtime-color]
                 #:dh [dh 0]
                 p)
  (refocus (cc-superimpose (let ([p (inset p 1 0)])
                             (colorize
                              (filled-rounded-rectangle (pict-width p) 
                                                        (+ dh (pict-height p))
                                                        5)
                              color))
                           p)
           p))

(define (adj find dx dy)
  (lambda (a b)
    (define-values (x y) (find a b))
    (values (+ x dx) (+ y dy))))
