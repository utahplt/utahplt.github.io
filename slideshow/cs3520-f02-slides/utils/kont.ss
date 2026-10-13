(module kont (lib "slideshow.ss" "texpict")

  (require "colors.ss"
	   "utils.ss"
	   (all-except "alg.ss" steps)
	   (lib "list.ss"))

  (provide eval-step eval-step* kont-step set-wide! steps)
  
  (define wide? #t)
  
  (define (set-wide! v) (set! wide? v))
  
  (define (line title . v)
    (let ([ti (colorize (bt title) BlueColor)]
          [sp (t " ")])
      (ht-append
       ti
       sp
       (apply (if wide? para para*)
              (- client-w (pict-width ti) (pict-width sp))
              (map alg-code v)))))
  
  (define (eval-step* e env)
    (vl-append
     line-sep
     (line "exp=" e)
     (line "env=" env)))
  
  (define (eval-step e env . k)
    (vl-append
     line-sep
     (line "exp=" e)
     (line "env=" env)
     (apply line "todo=" k)))
  
  (define (kont-step v . k)
    (vl-append
     line-sep
     (line "val=" v)
     (apply line "todo=" k)))
  
  (define (steps . l)
    (cons (list (car l))
          (let loop ([l l])
            (if (null? (cddr l))
                (list l)
                (cons (list (car l) (cadr l))
                      (loop (cdr l))))))))
