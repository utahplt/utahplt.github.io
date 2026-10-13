#lang racket

(define (hash-dir)
  (define sha1s
    (for/list ([f (in-directory)]
               #:when (file-exists? f))
      (call-with-input-file f sha1-bytes)))
  (sha1-bytes (apply bytes-append (sort sha1s bytes<?))))

(define M 1)

;; fill filesystem cache
(void (hash-dir))

(collect-garbage)
(time (for/list ([i M])
        (hash-dir)))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              (lambda () (hash-dir))))))

(collect-garbage)
(time (map thread-wait
           (for/list ([i M])
             (thread
              #:keep 'results
              #:pool 'own
              (lambda () (hash-dir))))))

(collect-garbage)
(time (map touch
           (for/list ([i M])
             (future
              (lambda () (hash-dir))))))
