(define Shape (interface () draw))
     
(define Rectangle
  (class* object% (Shape) (width height)
    (public
      [draw (lambda (dc x y)
	      (send dc draw-rectangle x y width height))])
    (sequence (super-init))))
    
(define Circle
  (class* object% (Shape) (radius)
    (public
      [draw (lambda (dc x y)
	      (send dc draw-ellipse
		    (- x radius)
		    (- y radius)
		    (* 2 radius)
		    (* 2 radius)))])
    (sequence (super-init))))
    
(define Translated
  (class* object% (Shape) (orig-shape dx dy)
    (public
      [draw (lambda (dc x y)
              (send orig-shape draw 
                    dc (+ x dx) (+ y dy)))])
    (sequence (super-init))))
