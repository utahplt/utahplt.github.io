#cs
(module lec14 (lib "slideshow.ss" "slideshow")
  (require "code.ss")

  (provide filter-nums-defn)

  (define filter-nums-defn
    (code
     (code:contract filter-nums :  (num -> bool) list-of-num
                    -> list-of-num)
     (define (filter-nums PRED l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r 
                    (filter-nums PRED (rest l)))]
            (cond
              [(PRED (first l)) 
               (cons (first l) r)]
              [else r]))])))))
