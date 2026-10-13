(define Basic-Shapes
  (unit (import) 
        (export Shape Rectangle Circle Translated)
        
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
         (sequence (super-init))))))

(define Gui
  (unit (import Shape canvas% frame%)
        (export display-shape)
    
    (define shape-canvas%
      (class canvas% (parent shape)
	(inherit get-dc)
	(override
	  [on-paint
	   (lambda ()
	     (send shape draw (get-dc) 0 0))])
	(sequence (super-init parent))))
    
    (define display-shape
      (lambda (a-shape)
	(unless (is-a? a-shape Shape)
	  (error 'display-shape "expected a Shape, got: ~e" a-shape))
	(let* ([frame (make-object frame% "Shapes" #f 150 150)]
	       [canvas (make-object shape-canvas% frame a-shape)])
	  (send frame show #t))))))

(define Picture 
  (unit (import Rectangle Circle Translated display-shape) 
        (export shape1 shape2)

    (define shape1 (make-object Rectangle 40 30))
    (define shape2 (make-object Translated 
		     (make-object Circle 20)
		     30 30))

    (display-shape shape1)
    (display-shape shape2)))

;; the graphics library (mred@) is defaultly a signed unit.
;; see mzscheme manual for details of signed units.
(define MrEd-Toolkit (unit/sig->unit mred@))

(define Basic-Program
  (compound-unit 
   (import) 
   (link [S (Basic-Shapes)] 
	 [M (MrEd-Toolkit)]
	 [G (Gui (S Shape) (M canvas%) (M frame%))] 
	 [P (Picture
	     (S Rectangle)
	     (S Circle)
	     (S Translated) 
	     (G display-shape))])
   (export)))

(invoke-unit Basic-Program)
