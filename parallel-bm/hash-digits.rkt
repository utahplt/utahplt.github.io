#lang racket

(define (hash-digs n)
  (define N (expt 10 n))
  (define p (open-output-bytes))
  (for ([i (in-range N)])
    (write-byte (+ (char->integer #\0) (modulo i 10)) p))
  (sha1-bytes (get-output-bytes p)))

(define M 8)
(define N 7)

(collect-garbage)
(time (for/list ([i M])
        (hash-digs N)))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              (lambda () (hash-digs N))))))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              #:pool 'own
              (lambda () (hash-digs N))))))

(collect-garbage)
(time (map touch
           (for/list ([i M])
             (future
              (lambda () (hash-digs N))))))
