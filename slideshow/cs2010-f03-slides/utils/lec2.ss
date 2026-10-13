#cs
(module lec2 (lib "slideshow.ss" "slideshow")
  (require "code.ss")

  (provide posn-def)

  (define posn-def
    (vl-append
     gap-size
     (page-para* "A" (code posn) "is")
     (page-para* (ghost bullet) (code (make-posn X Y)))
     (page-para* "where" (code X) "is a" (code num)
                 "and" (code Y) "is a" (code num)))))

   
