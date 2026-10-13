;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname friday-morning-lecture) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")))))
; A aquarium is either
;  * empty
;  * (cons num aquarium)

; feed-fish : aquarium num -> aquarium
; feed all the fish w amount of food
(check-expect (feed-fish (list 1 3 2) 5) (list 6 8 7))
(check-expect (feed-fish empty 5) empty)

;; Template
#;(define (feed-fish aq w)
  (cond
    [(empty? aq) ...]
    [(cons? aq)
     ...
     (first aq)
     (feed-fish (rest aq) w)
     ...]))

; We look at every fish in the list once
; O(n)
(define (feed-fish aq w)
  (cond
    [(empty? aq)
     empty]
    [(cons? aq)
     (cons
      (+ (first aq) w)
      (feed-fish (rest aq)
                 w))]))

; food-chain/so-far : aquarium num -> aquarium
; feeds each fish the fish to its left and the number so far
(check-expect (food-chain/so-far empty 0) empty)
(check-expect (food-chain/so-far (list 1 2 4) 0) (list 1 3 7))
(check-expect (food-chain/so-far (list 5) 0) (list 5))
(check-expect (food-chain/so-far (list 2 3) 0) (list 2 5))
(check-expect (food-chain/so-far (list 2 1) 0) (list 2 3))
(check-expect (food-chain/so-far (list 6) 20) (list 26))
(check-expect (food-chain/so-far (list 7 8) 2) (list 9 17))
(check-expect (food-chain/so-far empty 20) empty)

;;; Template
#;(define (food-chain/so-far aq sf)
  (cond
    [(empty? aq)
     ...]
    [(cons? aq)
     ...
     (first aq)
     (food-chain/so-far (rest aq) ... sf ...)
     ...]))

(define (food-chain/so-far aq sf)
  (cond
    [(empty? aq)
     empty]
    [(cons? aq)
     (cons
      (+ (first aq) sf)
      (food-chain/so-far (rest aq) 
                         (+ (first aq) sf)))]))

; food-chain : aquarium -> aquarium
; each fish has eaten all the fish to its left
(check-expect (food-chain empty) empty)
(check-expect (food-chain (list 1 2 4)) (list 1 3 7))
(check-expect (food-chain (list 5)) (list 5))
(check-expect (food-chain (list 2 3)) (list 2 5))
(check-expect (food-chain (list 2 1)) (list 2 3))

;; Template
#;(define (food-chain aq)
  (cond
    [(empty? aq) ...]
    [(cons? aq)
     ...
     (first aq)
     (food-chain (rest aq))
     ...]))

(define (food-chain-n2 aq)
  (cond
    [(empty? aq)
     empty]
    [(cons? aq)
     (cons (first aq)
           ; feed-fish looks at all the remaining ones
           (feed-fish
            ; food-chain also looks at the remaining ones
            (food-chain (rest aq))
            (first aq)))]))

(define (food-chain aq)
  (food-chain/so-far aq 0))

;;; 

; A nat is either
;  * 0
;  * (add1 nat)

; ! : nat -> nat
(define (! n)
  (cond
    [(zero? n)
     1]
    [else
     (* n (! (sub1 n)))]))

; !a : nat nat -> nat
(define (!a n a)
  (cond
    [(zero? n)
     a]
    [else
     (!a (sub1 n) (* n a))]))
