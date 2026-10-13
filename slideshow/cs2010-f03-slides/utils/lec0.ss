#cs
(module lec0 (lib "slideshow.ss" "slideshow")

  (require "colors.ss"
           "utils.ss"
           "code.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (provide rightarrow box-pict rwb image-steps anonymize-defn)

  (define rightarrow (hbl-append (tt " ") sym:rightarrow (tt " ")))
  
  (define (box-pict w h color box?)
    (code-align
     (dc (lambda (dc x y)
	   (let ([brush (send dc get-brush)]
		 [pen (send dc get-pen)])
	     (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
	     (send dc set-brush (send the-brush-list find-or-create-brush color 'solid))
	     (if box?
		 (send dc draw-rectangle x y w h)
		 (send dc draw-ellipse x y w h))
	     (send dc set-pen pen)
	     (send dc set-brush brush)))
	 w h 0 0)))
  
  (define rwb (lt-superimpose
               (box-pict 35 35 "red" #t)
               (box-pict 25 25 "blue" #f)))

  (define image-steps
    (vl-append
     line-sep
     (code (image=? (image+ #,(box-pict 35 35 "red" #t) #,(box-pict 25 25 "blue" #f)) #,rwb))
     (page-para* rightarrow (code (image=? #,rwb #,rwb)))
     (page-para* rightarrow (code true))))

  (define anonymize-defn
    (code (define (anonymize i)
            (offset-image+
             i
	     0 0 
             (filled-circle (image-width i) 
			    (image-height i)
			    'blue))))))


