#lang racket/base
(require racket/set racket/vector)

(define fixedSeedMersennePrime 2147483647)
(define seedForNotContained 12745)
(define I 20)
(define sizeOfTest 8)
(define Q 2000000)
(define maxNumber 4294967087)


(define-syntax times
  (syntax-rules ()
    [(_ e)
     (let loop ([v #f] [i I])
       (if (zero? i)
           v
           (loop (begin
                   (collect-garbage 'major)
                   (time e))
                 (sub1 i))))]))

;;create a map from random elements 
(define (createMapRandom size seed)
  (random-seed seed)
  (let loop ([ht (hasheq)] [i size])
    (if (zero? i)
        ht
        (let ([val (random maxNumber)])
          (if (hash-ref ht val #f)
              (loop ht i)
              (loop (hash-set ht  val  val)
                    (sub1 i)))))))

;;create a vector of random elements of length 'size'  
(define (vectorRand size seed)
  (let ([newVec (make-vector size)])
    (random-seed seed) ;;reset random 
    (for ([ind (in-range size)])
      (vector-set! newVec ind (random maxNumber)))
    newVec))


;;create a vector of length 'size'
;;using only 'firstN' elements of 'vc'
(define (getSubsetVector vc size firstN)
  (let ([newVec (make-vector size)])
    (for ([ind (in-range size)])
      (vector-set! newVec ind
                   (vector-ref vc (modulo ind firstN))))
    newVec))


        
(define vector8NotContained (vectorRand 8 seedForNotContained))
(define vector2NotContained (getSubsetVector vector8NotContained 8 2))
(define vector4NotContained (getSubsetVector vector8NotContained 8 4))

(define vector8Contained (vectorRand 8 fixedSeedMersennePrime))
(define vector2Contained (getSubsetVector vector8Contained 8 2))
(define vector4Contained (getSubsetVector vector8Contained 8 4))


(define (chooseSize x isNotContained)
  (if isNotContained
      (cond
        [(equal? x 2) vector2NotContained]
        [(equal? x 4) vector4NotContained]
        [else vector8NotContained])
      (cond
        [(equal? x 2) vector2Contained]
        [(equal? x 4) vector4Contained]
        [else vector8Contained])))



;;____benchmarks_____

;;insert not contained
(displayln "insert not contained")
(for ([sz (in-range  1 24)])
  (displayln (expt 2 sz)) 
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let([notContained (chooseSize (expt 2 sz) #t)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-set ht (vector-ref notContained
                                              (- sizeOfTest i))
                               (vector-ref notContained
                                           (- sizeOfTest i)))
                     (sub1 i)))))))))

;;insert contained
(displayln "insert contained")
(for ([sz (in-range  1 24)])
  (displayln (expt 2 sz))
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let ([contained  (chooseSize (expt 2 sz) #f)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-set ht (vector-ref contained
                                              (- sizeOfTest i))
                               (vector-ref contained
                                           (- sizeOfTest i)))
                     (sub1 i)))))))))
 
;;remove not contained
(displayln "remove not contained")
(for ([sz (in-range 1 24)])
  (displayln (expt 2 sz))
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let ([notContained (chooseSize (expt 2 sz) #t)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-remove ht (vector-ref notContained
                                                 (- sizeOfTest i)))
                     (sub1 i)))))))))

 
;;remove contained
(displayln "remove contained")
(for ([sz (in-range 1 24)])
  (displayln (expt 2 sz)) 
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let ([contained  (chooseSize (expt 2 sz) #f)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-remove ht (vector-ref contained
                                                 (- sizeOfTest i)))
                     (sub1 i)))))))))


;;lookup not contained
(displayln "lookup not contained")
(for ([sz (in-range 1 24)])
  (displayln (expt 2 sz)) 
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let ([notContained (chooseSize (expt 2 sz) #t)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-ref ht
                               (vector-ref notContained
                                           (- sizeOfTest i))
                               #f)
                     (sub1 i)))))))))

;;lookup contained
(displayln "lookup contained") 
(for ([sz (in-range 1 24)])
  (displayln (expt 2 sz)) 
  (let ([ht (createMapRandom (expt 2 sz) fixedSeedMersennePrime)])
    (let ([contained  (chooseSize (expt 2 sz) #f)])
      (times
       (for ([i (in-range Q)])
         (let loop ([v #f] [i sizeOfTest])
           (if (zero? i)
               v
               (loop (hash-ref ht
                               (vector-ref contained
                                           (- sizeOfTest i))
                               #f)
                     (sub1 i)))))))))
