#lang racket

(define (hash-nums n)
  (define N (expt 10 n))
  (define ht (make-hash))
  (for ([i (in-range N)])
    (hash-set! ht (number->string i) i)
    (hash-remove! ht (number->string (- i 10))))
  (hash-count ht))

(define M 8)
(define N 6)

(collect-garbage)
(time (for/list ([i M])
        (hash-nums N)))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              (lambda () (hash-nums N))))))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              #:pool 'own
              (lambda () (hash-nums N))))))

(collect-garbage)
(time (map touch
           (for/list ([i M])
             (future
              (lambda () (hash-nums N))))))
