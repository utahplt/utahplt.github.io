;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wednesday-morning-lecture) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp")))))
; A vfamily is either
;  * empty
;  * (make-parent string string iworld vfamily vfamily)
(define-struct parent (name kind place child1 child2))

(define sue (make-parent "Sue" "cat" iworld1 empty empty))
(define frank (make-parent "Frank" "cat" iworld1 empty sue))
(define ming (make-parent "Ming" "cham" iworld2 empty empty))
(define joe (make-parent "Joe" "cat" iworld2 ming frank))

; Template for vfamily funs
#;(define (vfamily-fun a-vf ...)
    (cond
      [(empty? a-vf) ...]
      [(parent? a-vf)
       ...
       (parent-name a-vf)
       (parent-kind a-vf)
       (parent-place a-vf)
       (vfamily-fun (parent-child1 a-vf ...))
       (vfamily-fun (parent-child2 a-vf ...))
       ...]))

; vfamily-count : vfamily -> number
; To count the number of animals in a-vf
(check-expect (vfamily-count empty) 0)
(check-expect (vfamily-count joe) 4)
(define (vfamily-count a-vf)
  (cond
    [(empty? a-vf) 0]
    [(parent? a-vf)
     (+ 1
        (vfamily-count (parent-child1 a-vf))
        (vfamily-count (parent-child2 a-vf)))]))

; A vfamily-or-false is either
;  * fvamily
;  * false

; vfamily-find : vfamily string -> vfamily-or-false
(check-expect (vfamily-find empty "Sue") false)
(check-expect (vfamily-find joe "Sue") sue)
(check-expect (vfamily-find joe "Larry") false)
(check-expect (vfamily-find joe "Ming") ming)

(define (vfamily-find a-vf who)
  (cond
    [(empty? a-vf) #f]
    [(parent? a-vf)
     (cond
       [(string=? who (parent-name a-vf))
        a-vf]
       [(parent? (vfamily-find (parent-child1 a-vf) who))
        (vfamily-find (parent-child1 a-vf) who)]
       [else
        (vfamily-find (parent-child2 a-vf) who)])]))