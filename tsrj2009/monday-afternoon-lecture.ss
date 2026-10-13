; A vcat is
;  (make-vcat number number)
(define-struct vcat (x happiness))

(define garfield (make-vcat 10 100))
(define hello-kitty (make-vcat 40 10))

; vcat-happy-enough? : vcat -> boolean
; To return true if the vcat's happiness is greater than or equal to 50
(check-expect (vcat-happy-enough? garfield) true)
(check-expect (vcat-happy-enough? hello-kitty) false)

(define (vcat-happy-enough? a-vcat)
  (>= (vcat-happiness a-vcat) 50))

; vcat-run : vcat -> vcat
; To advance the cat by 25 units
(check-expect (vcat-run (make-vcat 0 10))
              (make-vcat 25 10))
(check-expect (vcat-run (make-vcat 7 20))
              (make-vcat 32 20))
;(define (vcat-run a-vcat)
;  ...
;  (vcat-x a-vcat)
;  (vcat-happiness a-vcat)
;  ...)
(define (vcat-run a-vcat)
  (make-vcat
   (+ 25 (vcat-x a-vcat))
   (vcat-happiness a-vcat)))

; vcat-run-if-happy : vcat -> vcat
; Runs the cat if it is happy enough
(check-expect (vcat-run-if-happy (make-vcat 0 60))
              (make-vcat 25 60))
(check-expect (vcat-run-if-happy (make-vcat 24 31))
              (make-vcat 24 31))
;(define (vcat-run-if-happy a-vcat)
;  ...
;  a-vcat
;  (vcat-x a-vcat)
;  (vcat-happiness a-vcat)
;  ...)
(define (vcat-run-if-happy a-vcat)
  (cond
    [(vcat-happy-enough? a-vcat)
     (vcat-run a-vcat)]
    [else
     a-vcat]))

; A vcham is
;  (make-vcham string number)
(define-struct vcham (color happiness))

; vcham-happy-enough? : vcham -> boolean
; Returns true if the happiness is greater than 25
(check-expect (vcham-happy-enough? (make-vcham "red" 30))
              true)
(check-expect (vcham-happy-enough? (make-vcham "orange" 12))
              false)
(check-expect (vcham-happy-enough? (make-vcham "purple" 25))
              false)
;(define (vcham-happy-enough? a-vcham)
;  ...
;  (vcham-color a-vcham)
;  (vcham-happiness a-vcham)
;  ...)
(define (vcham-happy-enough? a-vcham)
  (> (vcham-happiness a-vcham) 25))

; A vanimal is either
;  * a vcat
;  * a vcham

; vanimal-happy-enough? : vanimal -> boolean
; Return true if the animal is happy enough
(check-expect (vanimal-happy-enough? (make-vcat 5 60)) true)
(check-expect (vanimal-happy-enough? (make-vcat 4 30)) false)
(check-expect (vanimal-happy-enough? (make-vcham "red" 30)) true)
(check-expect (vanimal-happy-enough? (make-vcham "blue" 12)) false)

;(define (vanimal-happy-enough? a-vanimal)
;  (cond
;    [(vcat? a-vanimal)
;     (vcat-happy-enough? a-vanimal)
;     ...]
;    [(vcham? a-vanimal)
;     (vcham-happy-enough? a-vanimal)
;     ...]))

(define (vanimal-happy-enough? a-vanimal)
  (cond
    [(vcat? a-vanimal)
     (vcat-happy-enough? a-vanimal)]
    [(vcham? a-vanimal)
     (vcham-happy-enough? a-vanimal)]))

; vcat-scare : vcat -> vcat
; Reduces its happiness by 25
(check-expect (vcat-scare (make-vcat 5 40)) (make-vcat 5 15))
(check-expect (vcat-scare (make-vcat 5 20)) (make-vcat 5 0))
;(define (vcat-scare a-vcat)
;  ...
;  (vcat-x a-vcat)
;  (vcat-happiness a-vcat)
;  ...)
(define (vcat-scare a-vcat)
  (make-vcat
   (vcat-x a-vcat)
   (max 0 (- (vcat-happiness a-vcat) 25))))

; vcham-scare : vcham -> vcham
; Changes the color to orange
(check-expect (vcham-scare (make-vcham "orange" 25)) (make-vcham "orange" 25))
(check-expect (vcham-scare (make-vcham "yellow" 21)) (make-vcham "orange" 21))
;(define (vcham-scare a-vcham)
;  ...
;  (vcham-color a-vcham)
;  (vcham-happiness a-vcham)
;  ...)
(define (vcham-scare a-vcham)
  (make-vcham
   "orange"
   (vcham-happiness a-vcham)))

; vanimal-scare : vanimal -> vanimal
; Scares the vanimal where scaring a cat reduces its happiness and
scaring a chamleon orange
(check-expect (vanimal-scare (make-vcat 5 40))
              (make-vcat 5 15))
(check-expect (vanimal-scare (make-vcham "red" 40))
              (make-vcham "orange" 40))
(define (vanimal-scare a-vanimal)
  (cond
    [(vcat? a-vanimal)
     (vcat-scare a-vanimal)]
    [(vcham? a-vanimal)
     (vcham-scare a-vanimal)]))
