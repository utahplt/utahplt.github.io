#lang racket
(require slideshow/pict
         slideshow/code-pict
         "pict-style.rkt"
         "in-file.rkt")

(provide hours-slide
         test-submod-slide
         clock-slide)

(define-syntax-rule (hcode c ...)
  (scale (code c ...) 0.85))

(define rackunit-code (code rackunit))

(define first-test-code
  (code (check-equal 0 (seconds->hours 0))))

(define hours-test-code
  (code
   (require #,rackunit-code)
   #,first-test-code
   (check-equal 1 (seconds->hours 3600))
   (check-equal 42 (seconds->hours 151200))))

(define module+-code (code module+))

(define (hours-submod-code name #:encloud? [encloud? #f])
  (let-syntax ([module+ (make-code-transformer #'module+-code)])
    (code
     (module+ #,name
       #,(if encloud?
             (encloud hours-test-code
                      #:color new-color)
             hours-test-code)))))

(define hours-code
  (lc-superimpose
   (code
    (provide current-hours)
    code:blank
    (define (current-hours)
      (seconds->hours (current-seconds)))
    code:blank
    (define (seconds->hours s)
      (quotient s (* 60 60))))
   (blank (pict-width (hours-submod-code (code test))) 0)))

(define (hours-slide)
  (scale (mk-file
          #:name "hours"
          #:suffix "rkt"
          (hcode
           #,hours-code))
         paper-scale))

(define (test-submod-slide #:encloud? [encloud? #t])
  (scale (mk-file
          #:name "hours"
          #:suffix "rkt"
          (let* ([p (hcode
                     #,hours-code
                     code:blank
                     #,(hours-submod-code (code test) #:encloud? encloud?))])
            p))
         paper-scale))

(define (clock-slide)
  (scale (inset (mk-file
                 #:name "clock"
                 #:suffix "rkt"
                 (code "tick"))
                20 0 0 0)
         paper-scale))

(module+ main
  (require slideshow)
  (slide (clock-slide)))



