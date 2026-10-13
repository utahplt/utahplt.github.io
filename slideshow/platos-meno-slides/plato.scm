(module plato (lib "slideshow-run.ss" "texpict") 
  (require (lib "face.ss" "texpict"))
  (require (lib "mred.ss" "mred")) ; for message-box
  (require (lib "balloon.ss" "texpict"))
  
  
  
  (define main-rect-width 200)
  (define main-rect-height 200)
  (define blue-square (frame (colorize (filled-rectangle main-rect-width main-rect-height) "blue")))
  (define lw-10 10)
  (define lw-5 5)
  (define STR-BOYYES "BOY: Yes.")
  (define STR-BOY-FOUR "BOY: Four.")
  (define STR-BOYEIGHT "BOY: Eight.")
  
  (define STR-MENOYES "MENO: Yes.")
  (define STR-BOYIDO "BOY: Yes, I do.")
  
  (define (dc-draw-nested-diamonds sq-side-length)
    (dc-draw-nested-diamonds-helper sq-side-length (ghost (rectangle sq-side-length sq-side-length))))       
  
  (define (dc-draw-nested-diamonds-helper sq-side-length pict-so-far)
    (if (= sq-side-length 0)
        pict-so-far
        (begin
          (let ((curr-pict (cc-superimpose (dc (dc-draw-diamond sq-side-length) sq-side-length sq-side-length 0 0) pict-so-far)))
           (dc-draw-nested-diamonds-helper (- sq-side-length 1) curr-pict)))))
  
  (define (dc-draw-nested-nw-half-sq-triangles sq-side-length)
    (dc-draw-nested-nw-half-sq-triangles-helper sq-side-length (ghost (rectangle sq-side-length sq-side-length))))
 
  (define (dc-draw-nested-nw-half-sq-triangles-helper sq-side-length pict-so-far)
    (let* ((curr-color (if (even? sq-side-length)
               "red"
               "green")))
    (if (= sq-side-length 0)
        pict-so-far
        (begin
          (let ((curr-pict (rt-superimpose (colorize (dc (dc-draw-nw-half-sq-triangle sq-side-length) sq-side-length sq-side-length 0 0) curr-color) pict-so-far)))
            (dc-draw-nested-nw-half-sq-triangles-helper (- sq-side-length 1) curr-pict))))))
  
  (define (dc-draw-nested-ne-half-sq-triangles sq-side-length)
    (dc-draw-nested-ne-half-sq-triangles-helper sq-side-length (ghost (rectangle sq-side-length sq-side-length))))
  
  (define (dc-draw-nested-ne-half-sq-triangles-helper sq-side-length pict-so-far)
    (let* ((curr-color (if (even? sq-side-length)
                           "red"
                           "green")))
      (if (= sq-side-length 0)
          pict-so-far
          (begin
            (let ((curr-pict (lt-superimpose (colorize (dc (dc-draw-ne-half-sq-triangle sq-side-length) sq-side-length sq-side-length 0 0) curr-color) pict-so-far)))
              (dc-draw-nested-ne-half-sq-triangles-helper (- sq-side-length 1) curr-pict))))))
  
  (define (dc-draw-nested-sw-half-sq-triangles sq-side-length)
    (dc-draw-nested-sw-half-sq-triangles-helper sq-side-length (ghost (rectangle sq-side-length sq-side-length))))
  
  (define (dc-draw-nested-sw-half-sq-triangles-helper sq-side-length pict-so-far)
    (let* ((curr-color (if (even? sq-side-length)
                           "red"
                           "green")))
      (if (= sq-side-length 0)
          pict-so-far
          (begin
            (let ((curr-pict (rt-superimpose (colorize (dc (dc-draw-sw-half-sq-triangle sq-side-length) sq-side-length sq-side-length 0 0) curr-color) pict-so-far)))
              (dc-draw-nested-sw-half-sq-triangles-helper (- sq-side-length 1) curr-pict))))))
  
  (define (dc-draw-nested-se-half-sq-triangles sq-side-length)
    (dc-draw-nested-se-half-sq-triangles-helper sq-side-length (ghost (rectangle sq-side-length sq-side-length))))
  
  (define (dc-draw-nested-se-half-sq-triangles-helper sq-side-length pict-so-far)
    (let* ((curr-color (if (even? sq-side-length)
                           "red"
                           "green")))
      (if (= sq-side-length 0)
          pict-so-far
          (begin
            (let ((curr-pict (lt-superimpose (colorize (dc (dc-draw-se-half-sq-triangle sq-side-length) sq-side-length sq-side-length 0 0) curr-color) pict-so-far)))
              (dc-draw-nested-se-half-sq-triangles-helper (- sq-side-length 1) curr-pict))))))
  
  
  ;;BEGINNING OF SLIDESHOW
  (slide/title
   "Plato's Meno and the Socratic Method"
   (page-para/c "slideshow arranged by Daniel Lyle")
   (page-para/c "May 2004")
   ) 
  
  (let* ((sq1 (lt-superimpose blue-square (colorize (filled-rectangle main-rect-width lw-10) "red")))
         (sq2 (rb-superimpose sq1 (colorize (filled-rectangle lw-10 main-rect-width) "red")))
         (sq3 (rb-superimpose sq2 (colorize (filled-rectangle main-rect-width lw-10) "red")))
         (sq4 (lt-superimpose sq3 (colorize (filled-rectangle lw-10 main-rect-width) "red")))) 
    (slide/title
     ""
     (vl-append 20 
                (t "SOCRATES: Now boy, you know that a square is a figure like this?")
                blue-square
                (t STR-BOYYES)
                (t "SOCRATES: It has all these four sides equal?"))
     'alts
     (list (list blue-square)
           (list sq1)
           (list sq2)
           (list sq3)
           (list sq4))))
  
  
  (define quartered-square (lc-superimpose  (ct-superimpose blue-square (colorize (filled-rectangle lw-5 main-rect-width) "black")) (colorize (filled-rectangle main-rect-width lw-5) "black")))
  
  (slide/title
   ""
   (page-para "SOCRATES: And these lines which go through the middle of it are also equal?")
   quartered-square
   (page-para STR-BOYYES)
   (page-para "SOCRATES: Such a figure could be either larger or smaller, could it not?")
   (page-para STR-BOYYES)) 
  
  (require (lib "class.ss")) ; to demonstrate dc
  (require "simple-shapes.scm")
 (require (lib "step.ss" "slideshow"))
 
  
  (let* ((side-length 200)
         (qsl (/ side-length 2))
        (marker-width 20)
        (sq (dc (dc-draw-square qsl) qsl qsl 0 0))
        (two-sq (vc-append sq sq)))
   (with-steps
     (v-dim h-dim area example response)
     (slide/title
      "" 
     (vr-append 10 
      (hc-append 10          
       ((vafter v-dim)
        (dc (dc-draw-v-dim-marker "2" side-length marker-width)
               marker-width side-length 0 0))
       (hc-append 0
        ((vafter v-dim)
         two-sq)
        ((vbetween v-dim area)
         two-sq)))
     (lc-superimpose
      ((vbetween h-dim area)
         (dc (dc-draw-h-dim-marker "2" side-length marker-width) 
           side-length marker-width 0 0)
         )
      ((vafter example)
       (dc (dc-draw-h-dim-marker "1" (/ side-length 2) marker-width) (/ side-length 2) marker-width 0 0)))
      )
    (lt-superimpose
     ((vonly v-dim)
      (t "SOCRATES: Now if this side is two feet long,"))
     ((vonly h-dim)
      (t "SOCRATES: and this side the same,"))
     ((vonly area)
      (page-para "SOCRATES: how many feet will the whole be?"))
     ((vafter example)
      (page-para "SOCRATES: Put it this way.  If it were two feet in this direction and only one in that, must not the area be two feet taken once?")
     )) 
     ((vonly response)
      (page-para STR-BOYYES)
     ))))
  
  (let* ((side-length 100)
         (qsl (/ side-length 2))
         (sq1 (dc (dc-draw-square-with-txt qsl "1") qsl qsl 0 0))
         (sq2 (dc (dc-draw-square-with-txt qsl "2") qsl qsl 0 0))
         (sq3 (dc (dc-draw-square-with-txt qsl "3") qsl qsl 0 0))
         (sq4 (dc (dc-draw-square-with-txt qsl "4") qsl qsl 0 0))
         (two-sq1 (vc-append sq1 sq2))
         (two-sq2 (vc-append sq3 sq4))
         (four-sq-ft-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0)))
  (with-steps
  (apart together1 together2) 
  (slide/title 
   ""
   (cc-superimpose
    ((vonly apart)
     (hc-append 200 two-sq1 two-sq1))
    ((vonly together1)
     (hc-append two-sq1 two-sq2))
    ((vonly together2)
     four-sq-ft-sq)
    )
    ((vafter apart)
     (page-para "SOCRATES: And how many feet is twice two? Work it out and tell me."))
    ((vafter together1)
     (page-para STR-BOY-FOUR))
    
     )))
  
  (let* ((side-length 100)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5)
  (slide/title
   ""
   (hc-append 20
     (lb-superimpose
      ((vafter step1)
       small-sq)
      ((vafter step2)
        big-sq))
      ((vafter step5)
       (dc (dc-draw-v-dim-marker "?" double-side-length marker-width) marker-width double-side-length 0 0))) 
    ((vafter step1)
    (page-para "SOCRATES: Now could one draw another figure double the size of this, but similar, that is with all its sides equal like this one?"))
    ((vafter step2)
     (page-para STR-BOYYES))
    ((vafter step3)
     (page-para "SOCRATES: How many feet will its area be?"))
    ((vafter step4)
     (page-para STR-BOYEIGHT))
    ((vafter step5)
     (page-para "SOCRATES: Now then, try to tell me how long each of its sides will be.  The present figure has a side of two feet.  What will be the side of the double-sized one?")))))
  
  (slide/title
   ""
   (page-para "BOY: It will be double, Socrates, obviously.")
   'next
   (page-para "SOCRATES: You see, Meno, that I am not teaching him anything, only asking.  Now he thinks he knows the length of the side of the eight-feet square.")
   'next
  (page-para STR-MENOYES)
  'next
  (page-para "SOCRATES: But does he?")
  'next
  (page-para "MENO: Certainly not.")
  'next
  (page-para "SOCRATES: He thinks it is twice the length of the other.")
  'next
  (page-para STR-MENOYES)
  'next
  (page-para "SOCRATES:  Now watch how he recollects things in order -- the proper way to recollect."))
  
  
 (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0)))
 (with-steps
  (step1 step2 step3 step4)
 (slide/title 
   ""
 (vl-append  
  (vl-append
   (rb-superimpose
    (lb-superimpose
     ((vafter step1)
      (lb-superimpose small-sq (ghost big-sq)))
       ((vafter step3)
         (linewidth line-width
          (colorize 
           (dc (lambda(dc dx dy) 
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue"))))
      ((vafter step4)
       (colorize
        (dc (lambda(dc dx dy)
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue")))
      ((vafter step3)
        (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0)))
      ((vafter step4)
        (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0)))
   ((vafter step1)
    (page-para "SOCRATES:  You say that the side of double length produces the double-sized figure?  Like this I mean, not long this way and short that.  It must be equal on all sides like the first figure, only twice its size, that is eight feet.  Think a moment whether you still expect to get it from doubling the side."
    ))
   ((vafter step2)
    (page-para STR-BOYIDO)) 
   (page-para ((vafter step3)
                (t "SOCRATES: Well now, shall we have a line double the length of this "))
              ((vafter step4)
                (t "if we add another the same length at this end?")))
   
   )))
  
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5)
  (slide/title
   ""
    (vl-append  
     (vl-append
      (rb-superimpose
       (lb-superimpose
        (lb-superimpose small-sq 
                        ((vafter step4)
                         big-sq))
         (linewidth line-width
          (colorize 
           (dc (lambda(dc dx dy) 
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue")))
        (colorize
         (dc (lambda(dc dx dy)
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue"))
        (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0))
        (cc-superimpose
         ((vafter step1)
          (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0))
         ((vafter step2)
           (colorize
            (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0)  "red"))))
   
   ((vafter step1)
    (page-para STR-BOYYES))
   ((vafter step2)
    (page-para "SOCRATES:  It is on this line then, according to you, that we shall make the eight-feet square, by taking four of the same length?"))
   ((vafter step3)
    (page-para STR-BOYYES))
   ((vafter step4)
    (page-para "SOCRATES:  Let us draw in four equal lines using the first as a base.  Does this not give us what you call the eight-feet figure?"))
   ((vafter step5)
    (page-para "BOY: Certainly."))
   )))
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5 step6)
  (slide/title
   ""
   (vl-append  
     (vl-append
      (rb-superimpose
       (lb-superimpose
        (rb-superimpose small-sq
         (rt-superimpose small-sq
          (lt-superimpose small-sq
           (lb-superimpose small-sq big-sq))))
         (linewidth line-width
          (colorize 
           (dc (lambda(dc dx dy) 
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue")))
        (colorize
         (dc (lambda(dc dx dy)
            (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue"))
        (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0))
        ((vafter step1)
          (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0)))
   ((vafter step1)
    (page-para "SOCRATES: But does it contain these four squares, each equal to the original four-feet one?"))
   ((vafter step2)
    (page-para STR-BOYYES))
   ((vafter step3)
    (page-para "SOCRATES: How big is it then?  Won't it be four times as big?"))
   ((vafter step4)
    (page-para "BOY: Of course."))
   ((vafter step5)
    (page-para "SOCRATES: And is four times the same as twice?"))
   ((vafter step6)
    (page-para "BOY: Of course not.")))))
  
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0))
         (big-sq-with-txt (dc (dc-draw-square-with-txt double-side-length "16 square feet") double-side-length double-side-length 0 0)))
    (with-steps 
     (step1 step2 step3 step4 step5)
    (slide/title
     ""
     (lt-superimpose
      (vl-append  
        (vl-append
         (rb-superimpose
          (lb-superimpose 
           ((vbetween step1 step2)
            (rb-superimpose small-sq
             (rt-superimpose small-sq
              (lt-superimpose small-sq
               (lb-superimpose small-sq big-sq)))))
           ((vbetween step1 step2)
            (linewidth line-width
             (colorize 
               (dc (lambda(dc dx dy) 
                (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue"))))
          ((vbetween step1 step2)
           (colorize
            (dc (lambda(dc dx dy)
              (send dc draw-line dx dy (+ dx side-length) dy)) side-length line-width 0 0) "blue")))
         ((vbetween step1 step2)
          (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0)))
        ((vafter step1)
         (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0)))
      ((vafter step3)
       (colorize big-sq-with-txt "blue")))
     ((vafter step1)
      (page-para "SOCRATES: So doubling the side has given us not a double but a fourfold figure?"))
     ((vafter step2)
      (page-para "BOY: True."))
     ((vafter step3)
      (page-para "SOCRATES: And four times four are sixteen, are they not?"))
     ((vafter step4)
      (page-para STR-BOYYES))
     ((vafter step5)
      (page-para "SOCRATES: Then how big is the side of the eight-feet figure?  This one has given us four times the original area, hasn't it?")))))
  
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (small-sq-with-txt (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (side-length-marker (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0))
         (double-side-length-marker (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0))
         (big-sq-with-txt (dc (dc-draw-square-with-txt double-side-length "16 square feet") double-side-length double-side-length 0 0)))
    (with-steps 
     (step1 step2 step3 step4 step5)
     (slide/title
     ""
     (vl-append
      (lb-superimpose
       ((vafter step3)
           small-sq)
       (lb-superimpose
        (lb-superimpose
         (lb-superimpose
          (lb-superimpose
           ((vonly step1)
            big-sq-with-txt)
           ((vafter step2)
            big-sq))
           ((vonly step3)
            (colorize small-sq "red")))
          ((vonly step2)
            (colorize small-sq-with-txt "red")))
          ((vonly step4)
           (colorize big-sq "red")))
          )
        ((vonly step2)
         side-length-marker))
        
     
     ((vafter step1)
      (page-para STR-BOYYES))
     ((vafter step2)
      (page-para "SOCRATES: And a side half the length gave us a square of four feet?"))
     (page-para 
       ((vafter step3) (t "SOCRATES: Good. And isn't a square of eight feet double this one"))
       ((vafter step4) (t " and half that?")))
     ((vafter step5)
      (page-para STR-BOYYES))
       
     )))
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (small-sq-with-txt (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (side-length-marker (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0))
         (double-side-length-marker (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0))
         (big-sq-with-txt (dc (dc-draw-square-with-txt double-side-length "16 square feet") double-side-length double-side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5 step6 step7)
  (slide/title
   ""
   (vl-append
    (vl-append
     (lb-superimpose
       small-sq
       big-sq)
     (lt-superimpose
      (lt-superimpose
        side-length-marker
        ((vonly step1)
         (colorize side-length-marker "red")))
      ((vonly step5)
       (colorize side-length-marker "red"))))
    (lt-superimpose
      (lt-superimpose
        double-side-length-marker
        ((vonly step2)
         (colorize double-side-length-marker "red")))
      ((vonly step6)
       (colorize double-side-length-marker "red"))))
   (page-para 
      ((vafter step1)
       (t "SOCRATES: Will it not have a side greater than this one"))  
      ((vafter step2)
       (t "and less than that?")))
   (page-para
      ((vafter step3)
       (t "BOY: I think it will.")))
   (page-para
      ((vafter step4)
       (t "SOCRATES: Right. Always answer what you think.")))
   (page-para
      ((vafter step5)
       (t "SOCRATES: Now tell me: was not this side two feet long,"))
      ((vafter step6)
       (t "and this one four?")))
   (page-para
    ((vafter step7)
     (t STR-BOYYES)))
      
   )))
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (small-sq-area (* side-length side-length))
         (dbl-small-sq-area-side-length (sqrt (* 2 small-sq-area)))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (small-sq-with-txt (dc (dc-draw-square-with-txt side-length "4 square feet") side-length side-length 0 0))
         (side-length-marker (dc (dc-draw-h-dim-marker "2" side-length marker-width) side-length marker-width 0 0))
         (double-side-length-marker (dc (dc-draw-h-dim-marker "4" double-side-length marker-width) double-side-length marker-width 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0))
         (unknown-side-length-marker (dc (dc-draw-h-dim-marker "?" dbl-small-sq-area-side-length marker-width) dbl-small-sq-area-side-length marker-width 0 0)))
  (slide/title
   ""
   (vl-append
    (vl-append
     (vl-append
      (lb-superimpose
       big-sq
       small-sq)
      side-length-marker)
     double-side-length-marker)
    unknown-side-length-marker)
   (page-para "SOCRATES: Then the side of the eight-feet figure must be longer than two feet but shorter than four?")
   'next
   (page-para "BOY: It must.")
   'next
   (page-para "SOCRATES: Try to say how long you think it is.")
   'next
   (page-para "BOY: Three feet.")
   ))
  
  
  (let* ((side-length 100)
        (line-width 1)
         (marker-width 20)
         (double-side-length (* side-length 2))
         (one-and-half-side-length (/ (* 3 side-length) 2))
         (small-sq-area (* side-length side-length))
         (dbl-small-sq-area-side-length (sqrt (* 2 small-sq-area)))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (big-sq (dc (dc-draw-square double-side-length) double-side-length double-side-length 0 0))
         (nine-ft-sq (dc (dc-draw-square one-and-half-side-length) one-and-half-side-length one-and-half-side-length 0 0))
         (red-v2ft (colorize (filled-rectangle line-width side-length) "red"))
         (red-v3ft (colorize (filled-rectangle line-width one-and-half-side-length) "red"))
         (red-h3ft (colorize (filled-rectangle one-and-half-side-length line-width) "red"))
         (two-marker (dc (dc-draw-v-dim-marker "2" side-length marker-width) marker-width side-length 0 0))
         (one-marker (dc (dc-draw-v-dim-marker "1" (/ side-length 2) marker-width) marker-width (/ side-length 2) 0 0))
         (h3-marker (dc (dc-draw-h-dim-marker "3" one-and-half-side-length marker-width) one-and-half-side-length marker-width 0 0))
         (v3-marker (dc (dc-draw-v-dim-marker "3" one-and-half-side-length marker-width) marker-width one-and-half-side-length 0 0))
         )
  (with-steps
   (step1 step2 step3 step4 step5 step6 step7 step8 step9 step10)
  (slide/title
   ""
    (hb-append
     (vc-append
      (vc-append
       ((vafter step3)
        one-marker)
       ((vafter step2)
        two-marker))
      (ghost (rectangle marker-width marker-width)))
     (vl-append
      (lb-superimpose
       (lb-superimpose
        (lb-superimpose
         (lb-superimpose 
          (lb-superimpose
           big-sq
           small-sq)
          ((vafter step5)
            (colorize nine-ft-sq "red")) 
          ((vonly step4)
           red-h3ft))
          ((vbetween step1 step3)
           red-v2ft))
          ((vbetween step3 step4)
           red-v3ft)))
        ((vafter step7) 
         h3-marker)))  
      
   (page-para 
    ((vafter step1)
     (t "SOCRATES: If so, shall we add half of this bit and make it three feet?"))
    ((vafter step2)
     (t"Here are two,"))
    ((vafter step3)
     (t "and this is one,"))
    ((vafter step4)
     (t"and on this side similarly we have two plus one;"))
    ((vafter step5)
     (t "and here is the figure you want.")))
    (page-para 
    ((vafter step6)
     (t STR-BOYYES)))
    (page-para
     ((vafter step7)
      (t "SOCRATES:  If it is three feet this way,"))
     ((vafter step8)
      (t "and three that,"))
     ((vafter step9)
      (t "will the whole"))
     ((vafter step9)
      (t "area be three times three feet?")))
    ((vafter step10)
     (page-para "BOY: It looks like it."))
   )))
  
  (slide/title
   ""
   (page-para "SOCRATES: And that is how many?")
   (page-para "BOY: Nine.")
   (page-para "SOCRATES: Whereas the square double our first square had to be how many?")
   (page-para "BOY: Eight.")
   (page-para "SOCRATES: But we haven't yet got the square of eight feet even from a three-feet side?")
   (page-para "BOY: It's no use, Socrates, I just don't know.")
  )
  
  (slide/title
   ""
   (page-para "SOCRATES: Observe, Meno, the stage he has reached on the path of recollection.  At the beginning he did not know the side of the square of eight feet.  Nor indeed does he know it now, but then he thought he knew it and answered boldly, as was appropriate -- he felt no perplexity.  Now however he does feel perplexed.  Not only does he not know the answer; he doesn't even think he knows.")
  (page-para "MENO: Quite true.")
  (page-para "SOCRATES: Isn't he in a better position now in relation to what he didn't know?") 
  (page-para "MENO: I admit that too.")
  (page-para "SOCRATES: So in perplexing him and numbing him like the sting-ray, have we done him any harm?")
  (page-para "MENO: I think not."))
  
  (slide/title
   ""
  (page-para "SOCRATES: In fact we have helped him to some extent towards finding out the right answer, for now not only is he ignorant of it but he will be glad to look for it.  Up to now, he thought he could speak well and fluently, on many occasions and before large audiences, on the subject of a square double the size of a given square, maintaining that it must have a side of double the length.")
  (page-para "MENO: No doubt.")
  (page-para "SOCRATES: Do you suppose then that he would have attempted to look for, or learn, what he thought he knew (though he did not), before he was thrown into perplexity, became aware of his ignorance, and felt a desire to know?")
  (page-para "MENO: No.")
  (page-para "SOCRATES: Then the numbing process was good for him.")
  (page-para "MENO: I agree."))
  
  (slide/title
   ""
  (page-para "SOCRATES: Now notice what, starting from this state of perplexity, he will discover by seeking the truth in company with me, though I simply ask him questions without teaching him.  Be ready to catch me if I give him any instruction or explanation instead of simply interrogating him on his own opinions."))
  
  (let* ((side-length 100)
         (line-width 1)
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5 step6)
  (slide/title
   ""
   (lb-superimpose
    (vr-append
     (hc-append
     ((vafter step1)
      small-sq)
     ((vafter step4)
      small-sq))
     ((vafter step5)
      small-sq))
    ((vafter step6)
    small-sq))
   ((vafter step1)
    (page-para "SOCRATES: Tell me, boy, is not this our square of four feet?"))
   ((vafter step2)
    (page-para "SOCRATES: You understand?"))
   ((vafter step3)
    (page-para STR-BOYYES))
   ((vafter step4)
    (page-para "SOCRATES: Now we can add another equal to it like this?"))
   ((vafter step5)
    (page-para "SOCRATES: And a third here, equal to each of the others?"))
   ((vafter step6)
    (page-para "SOCRATES: And then we can fill in this one in the corner?")))));;45
   
  (let* ((side-length 100)
         (line-width 1)
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0)))
  (with-steps
   (step1 step2 step3 step4 step5 step6)
  (slide/title
   ""
   (lb-superimpose
    (vr-append
     (hc-append
      small-sq
      small-sq)
      small-sq)
    small-sq)
   
   ((vafter step1)
    (page-para STR-BOYYES))
   ((vafter step2)
    (page-para "SOCRATES: Then here we have four equal squares?"));;46
   ((vafter step3)
    (page-para STR-BOYYES))
   ((vafter step4)
    (page-para "SOCRATES: And how many times the size of the first square is the whole?"))
   ((vafter step5)
    (page-para "BOY: Four times."))
   ((vafter step6)
    (page-para "SOCRATES:  And we want one double the size.  You remember?")))))
  
  (let* ((side-length 100)
         (line-width 1)
         (dbl-side-length (* 2 side-length))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (four-sqrs (lb-superimpose 
                    (vr-append
                     (hc-append
                      small-sq
                      small-sq)
                     small-sq)
                    small-sq))
         )
  (with-steps
   (step1 step2 step3 step4 step5 step6)
  (slide/title
   ""
   (cc-superimpose
    (lt-superimpose
     four-sqrs
    ((vafter step2)
     (colorize (dc (dc-draw-diamond dbl-side-length) dbl-side-length dbl-side-length 0 0) "blue")))
    ((vafter step6)
     (colorize (dc-draw-nested-diamonds dbl-side-length) "blue")))
   ((vafter step1)
    (page-para STR-BOYYES))
   ((vafter step2)
    (page-para "SOCRATES: Now does this line going from corner to corner cut each of these squares in half?"))
   ((vafter step3)
    (page-para STR-BOYYES))
   ((vafter step4)
    (page-para "SOCRATES: And these are four equal lines enclosing this area?"))
   ((vafter step5)
    (page-para "BOY: They are."))
   ((vafter step6)
    (page-para "SOCRATES: Now think.  How big is this area?")))))
  
  
  
  
  (let* ((side-length 100)
         (line-width 1)
         (dbl-side-length (* 2 side-length))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (four-sqrs (lb-superimpose 
                    (vr-append
                     (hc-append
                      small-sq
                      small-sq)
                     small-sq)
                    small-sq))
         )
    (with-steps
     (step1 step2 step3 step4 step5 step6 step7 step8 step9)
  (slide/title
   ""
   (rb-superimpose
   (lb-superimpose
   (rb-superimpose
    (lb-superimpose
     (cc-superimpose
      (lt-superimpose
       four-sqrs
       (colorize (dc (dc-draw-diamond dbl-side-length) dbl-side-length dbl-side-length 0 0) "blue"))
      ((vonly step1)
       (colorize (dc-draw-nested-diamonds dbl-side-length) "blue")))
     ((vafter step6)
      (dc-draw-nested-nw-half-sq-triangles side-length)))
    ((vafter step7)
     (dc-draw-nested-ne-half-sq-triangles side-length)))
   ((vafter step8)
    (dc-draw-nested-sw-half-sq-triangles side-length)))
   ((vafter step9)
    (dc-draw-nested-se-half-sq-triangles side-length)))
   
    ((vafter step1)
    (page-para "BOY: I don't understand."))
   ((vafter step2)
    (page-para "SOCRATES: Here are four squares.  Has not each line cut off the inner half of each of them?"))
   ((vafter step3)
    (page-para STR-BOYYES))
   ((vafter step4)
    (page-para "SOCRATES: And how many halves are there in this figure? (inside the blue diamond)"))
   ((vafter step5)
   (page-para "BOY: Four.")))))
  
  
  (let* ((side-length 100)
         (line-width 1)
         (dbl-side-length (* 2 side-length))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (four-sqrs (lb-superimpose 
                    (vr-append
                     (hc-append
                      small-sq
                      small-sq)
                     small-sq)
                    small-sq)))
  (with-steps
   (step1 step2 step3 step4 step5 step6)
  (slide/title
   ""
   (cc-superimpose
    (lb-superimpose
     (lt-superimpose
       four-sqrs
       (colorize (dc (dc-draw-diamond dbl-side-length) dbl-side-length dbl-side-length 0 0) "blue"))
      ((vonly step1)
       (colorize small-sq "red")))
      ((vafter step5)
       (colorize (dc-draw-nested-diamonds dbl-side-length) "blue")))
    ((vafter step1)   
    (page-para "SOCRATES: And how many in this one?"));;54
   ((vafter step2)
    (page-para "BOY: Two."))
   ((vafter step3)
    (page-para "SOCRATES: And what is the relation of four to two?"));;55
   ((vafter step4)
    (page-para "BOY: Double."))
   ((vafter step5)
    (page-para "SOCRATES: How big is this figure then?"))
   ((vafter step6)
    (page-para "BOY: Eight feet."))
   )))
  
  
  (let* ((side-length 100)
         (line-width 1)
         (dbl-side-length (* 2 side-length))
         (small-sq (dc (dc-draw-square side-length) side-length side-length 0 0))
         (four-sqrs (lb-superimpose 
                    (vr-append
                     (hc-append
                      small-sq
                      small-sq)
                     small-sq)
                    small-sq)))
  (with-steps
   (step1 step2 step3 step4 step5)
  
  (slide/title
   ""
   (lb-superimpose
    (lt-superimpose
       four-sqrs
       (colorize (dc (dc-draw-diamond dbl-side-length) dbl-side-length dbl-side-length 0 0) "blue"))
    ((vafter step2)
     (colorize (dc (lambda(dc dx dy)(send dc draw-line dx dy (+ dx side-length) (+ dy side-length))) side-length side-length 0 0) "red")))
      
   ((vafter step1)
    (page-para "SOCRATES: On what base?"))
   ((vafter step2)
    (page-para "BOY: This one."))
   ((vafter step3)
    (page-para "SOCRATES: The line which goes from corner to corner of the square of four feet?"))
   ((vafter step4)
    (page-para STR-BOYYES))
   ((vafter step5)
    (page-para "SOCRATES: The technical name for it is 'diagonal'; so if we use that name, it is your personal opinion that the square on the diagonal of the original square is double its area."));;59
  )))
  
  (slide/title
   ""
    (page-para "BOY: That is so, Socrates.")
    (page-para "SOCRATES: What do you think, Meno?  Has he answered with any opinions that were not his own?")
    (page-para "MENO: No, they were all his."))
  
  (slide/title
   "THE END"
   (page-para "References: Plato's Meno"))
  )