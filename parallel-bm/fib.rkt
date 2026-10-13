#lang racket
(require racket/future)

(define M 8)
(define N 40)

(define (fib n)
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(time (for/list ([i M])
        (fib N)))

(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              (lambda () (fib N))))))

(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              #:pool 'own
              (lambda () (fib N))))))

(time (map touch
           (for/list ([i M])
             (future
              (lambda () (fib N))))))
