#lang racket
(require racket/future)

(define (strfib* n)
  (cond
    [(= n 0) "1"]
    [(= n 1) "1"]
    [else (number->string
           (+ (string->number* (strfib* (- n 1)))
              (string->number* (strfib* (- n 2)))))]))

(define (string->number* s)
  ;; avoid dynamic lookup of parameters
  (string->number s
                  10
                  'number-or-false
                  'decimal-as-inexact
                  'double))

(define (strfib n)
  (cond
    [(= n 0) "1"]
    [(= n 1) "1"]
    [else (number->string
           (+ (string->number (strfib (- n 1)))
              (string->number (strfib (- n 2)))))]))

(define M 4)
(define N 32)

(collect-garbage)
(time (for/list ([i M])
        (strfib N)))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              (lambda () (strfib N))))))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              #:pool 'own
              (lambda () (strfib N))))))

(collect-garbage)
(time (map touch
           (for/list ([i M])
             (future
              (lambda () (strfib N))))))
