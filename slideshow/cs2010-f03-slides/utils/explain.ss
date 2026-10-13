#cs
(module explain (lib "slideshow.ss" "slideshow")
  (require (lib "balloon.ss" "texpict"))

  (provide explain explain*)

  (define (explain* fract p xp yp dir . l)
    (place-balloon (wrap-balloon 
                    (apply para (* client-w fract) l)
                    dir
                    (case dir
                      [(nw w sw) (- gap-size)]
                      [(s n) 0]
                      [(ne e se) gap-size])
                    (case dir
                      [(nw n ne) (- gap-size)]
                      [(w e) 0]
                      [(sw s se) gap-size]))
                   p
                   (* xp (pict-width p))
                   (* yp (pict-height p))))
  
  (define (explain p xp yp dir . l)
    (apply explain* 1/4 p xp yp dir l)))
