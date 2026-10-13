;; original shape datatype

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

;; original factory

(define Factory
  (class object% ()
    (public
      [make-circle
       (lambda (radius)
	 (make-object Circle radius))]
      [make-rectangle
       (lambda (width height)
	 (make-object Rectangle width height))]
      [make-translated
       (lambda (shape dx dy)
	 (make-object Translated shape dx dy))])
    (sequence (super-init))))

(define the-factory (make-object Factory))

;; variant extension

(define Union
  (class* object% (Shape) (left right)
    (public
      [draw (lambda (dc x y)
              (send left draw dc x y)
              (send right draw dc x y))])
    (sequence (super-init))))

;; variant extension factory

(define Trans-Factory
  (class Factory ()
    (public
      [make-union
       (lambda (left right)
	 (make-object Union left right))])
    (sequence
      (super-init))))

(set! the-factory (make-object Trans-Factory))

;; operation extension

(define BB-Shape (interface (Shape) bounding-box))

(define-struct BB (left top right bottom))
 
(define BB-Rectangle
  (class* Rectangle (BB-Shape) (width height)
    (public
      [bounding-box
       (lambda () (make-BB 0 0 width height))])
    (sequence (super-init width height))))
        
(define BB-Circle
  (class* Circle (BB-Shape) (r)
    (public
      [bounding-box 
        (lambda () (make-BB (- r) (- r) r r))])
    (sequence (super-init r))))
        
(define BB-Translated
  (class* Translated (BB-Shape) (shape dx dy)
    (public
      [bounding-box
       (lambda ()
	 (let ([pre-bb (send shape bounding-box)])
	   (make-BB (+ (BB-left pre-bb) dx)
		    (+ (BB-top pre-bb) dy)
		    (+ (BB-right pre-bb) dx)
		    (+ (BB-bottom pre-bb) dy))))])
    (sequence (super-init shape dx dy))))
        
(define BB-Union
  (class* Union (BB-Shape) (left right)
    (public
      [bounding-box
       (lambda ()
	 (let ([left-bb (send left bounding-box)]
	       [right-bb (send right bounding-box)])
	   (make-BB (min (BB-left left-bb) (BB-left right-bb))
		    (min (BB-top left-bb) (BB-top right-bb))
		    (max (BB-right left-bb) (BB-right right-bb))
		    (max (BB-bottom left-bb) (BB-bottom right-bb)))))])
    (sequence (super-init left right))))

;; operation extension factory

(define BB-Factory
  (class Trans-Factory ()
    (override
      [make-circle
       (lambda (radius)
	 (make-object BB-Circle radius))]
      [make-rectangle
       (lambda (width height)
	 (make-object BB-Rectangle width height))]
      [make-translated
       (lambda (shape dx dy)
	 (make-object BB-Translated shape dx dy))]
      [make-union
       (lambda (left right)
	 (make-object BB-Union left right))])
    (sequence
      (super-init))))

(set! the-factory (make-object BB-Factory))

;; client

(define shape-canvas%
  (class canvas% (panel shape)
    (inherit get-dc get-client-size)
    (override
      [on-paint
       (lambda ()
         (let-values ([(win-width win-height) (get-client-size)])
	   (let* ([bb (send shape bounding-box)]
                  [size
		   (lambda (left1 right1 left2 right2)
		     (- (/ (- (- right2 left2)
			      (- right1 left1))
			   2)
			left1))]
		  [x (size (BB-left bb) (BB-right bb) 0 win-width)]
		  [y (size (BB-top bb) (BB-bottom bb) 0 win-height)])
	     (send shape draw (get-dc) x y))))])
    (sequence (super-init panel))))
	
(define display-shape
  (lambda (a-shape)
    (unless (is-a? a-shape BB-Shape)
      (error 'display-shape "expected a BB-Shape, got: ~e" a-shape))
    (let* ([frame (make-object frame% "Centered Shapes" #f 150 150)]
	   [canvas (make-object shape-canvas% frame a-shape)])
      (send frame show #t))))

(display-shape 
  (send the-factory make-union
    (send the-factory make-rectangle 10 30)
    (send the-factory make-translated 
       (send the-factory make-circle 20) 30 30)))
