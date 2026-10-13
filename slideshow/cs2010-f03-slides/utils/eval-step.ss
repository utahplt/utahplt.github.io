(module eval-step (lib "slideshow.ss" "slideshow")
  (require "colors.ss"
           "utils.ss"
           "code.ss")

  (provide eval-steps eval-steps/prefix)

  (define (eval-steps title step expl . l)
    (apply eval-steps/prefix title 'nothing step expl l))

  (define (eval-steps/prefix title prefix step expl . l)
    (slide/title
     title
     prefix
     (page-para step)
     'next
     (page-para sym:rightarrow)
     (page-para (car l))
     (blank)
     (or expl (blank)))
    (unless (null? (cdr l))
      (let* ([first-step-top (+ (- client-h (pict-height titleless-page))
				(if (pict? prefix)
				    (+ (pict-height prefix) gap-size)
				    0))]
             [last-step-top (+ first-step-top
                               (pict-height step)
                               (pict-height sym:rightarrow)
                               (* 2 gap-size))])
        (scroll-transition 0 last-step-top
                           client-w
                           (pict-height (car l))
                           0 (- first-step-top last-step-top)
                           0.5 20))
      (apply eval-steps/prefix title prefix l))))