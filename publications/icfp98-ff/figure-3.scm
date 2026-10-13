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

(define shape-canvas%
  (class canvas% (parent a-shape)
    (inherit get-dc)
    (override
      [on-paint
       (lambda ()
	 (send a-shape draw (get-dc) 0 0))])
    (sequence (super-init parent))))
    
(define display-shape
  (lambda (a-shape)
    (unless (is-a? a-shape Shape)
      (error 'display-shape "expected a Shape, got: ~e" a-shape))
    (let* ([frame (make-object frame% "Shapes" #f 150 150)]
	   [canvas (make-object shape-canvas% frame a-shape)])
      (send frame show #t))))

(define shape1 (make-object Rectangle 40 30))
(define shape2 (make-object Translated 
                 (make-object Circle 20)
                 30 30))

(display-shape shape1)
(display-shape shape2)
