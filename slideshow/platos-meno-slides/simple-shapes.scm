(module simple-shapes mzscheme
  (require (lib "class.ss"))
  (require (lib "face.ss" "texpict"))
  (define (dc-draw-cross sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-side-x (+ dx sq-side-length))
            (south-side-y (+ dy sq-side-length))
            (half-side (/ sq-side-length 2))
            (mid-x (+ dx half-side))
            (mid-y (+ dy half-side)))
        (send dc draw-line mid-x dy mid-x south-side-y)
        (send dc draw-line dx mid-y east-side-x mid-y))))
  
  (define (dc-draw-nw-half-sq-triangle sq-side-length)
    (lambda (dc dx dy)
     (let* ((east-x (+ dx sq-side-length))
           (north-y (- dy sq-side-length)))
          (send dc draw-line dx dy east-x dy);;right edge
          (send dc draw-line east-x dy east-x north-y);;bottom edge
          (send dc draw-line dx dy east-x north-y);;diagonal
          )))
  
  (define (dc-draw-sw-half-sq-triangle sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-x (+ dx sq-side-length))
             (south-y (+ dy sq-side-length)))
        (send dc draw-line dx dy east-x dy)
        (send dc draw-line east-x dy east-x south-y)
        (send dc draw-line dx dy east-x south-y))))
  
  (define (dc-draw-ne-half-sq-triangle sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-x (+ dx sq-side-length))
             (north-y (- dy sq-side-length)))
        (send dc draw-line dx dy east-x dy);;bottom edge
        (send dc draw-line east-x dy dx north-y);;diagonal edge
        (send dc draw-line dx dy dx north-y);;left edge
        )))
  
  (define (dc-draw-se-half-sq-triangle sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-x (+ dx sq-side-length))
             (south-y (+ dy sq-side-length)))
        (send dc draw-line dx dy east-x dy)
        (send dc draw-line dx dy dx south-y)
        (send dc draw-line east-x dy dx south-y))))
             
             
  
  (define (dc-draw-diamond sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-side-x (+ dx sq-side-length))
            (south-side-y (+ dy sq-side-length))
            (half-side (/ sq-side-length 2))
            (mid-x (+ dx half-side))
            (mid-y (+ dy half-side)))
        (send dc draw-line dx mid-y mid-x dy);nw diamond edge
        (send dc draw-line mid-x dy east-side-x mid-y);;ne diamond edge
        (send dc draw-line east-side-x mid-y mid-x south-side-y) ;;se diamond edge
        (send dc draw-line mid-x south-side-y dx mid-y);;sw diamond edge
        )))
  
  (define (dc-draw-square sq-side-length)
    (lambda (dc dx dy)
      (let* ((east-side-x (+ dx sq-side-length))
             (south-side-y (+ dy sq-side-length)))
        (send dc draw-line dx dy east-side-x dy);north edge
        (send dc draw-line dx dy dx south-side-y);west edge
        (send dc draw-line east-side-x dy east-side-x south-side-y);east edge
        (send dc draw-line dx south-side-y east-side-x south-side-y);;south edge
        )))
  
  (define (dc-draw-square-with-txt sq-side-length txt)
    (dc-draw-square-with-txt-helper sq-side-length txt dc-draw-square))
  
  (define (dc-draw-square-with-txt-helper sq-side-length txt square-drawer)
    (lambda (dc dx dy)
      (let* ((char-width (send dc get-char-width))
             (text-width (* (string-length txt) char-width))
             (half-text-width (/ text-width 2))
             (half-text-height (send dc get-char-height))
             (half-side-length (/ sq-side-length 2)))
        
        (send dc draw-text txt (- (+ dx half-side-length) half-text-width) (+ dy half-side-length) #f 0 0)
        ((square-drawer sq-side-length) dc dx dy)
         )))
  
  
  ;;draw a dimension marker like on a drafting picture 
  ;;(a set of two arrows: one pointing at and bracketed by a short top line
  ;; and one pointing at and bracketed by a short bottom line, both arrows pointing away from text
  ;; denoting the length of the dimension this dimension marker is marking.)
  ;;@param dim-str the text to be used for this marker to denote the length of the dimension this dimension marker
  ;; is marking.
  ;;@param side-length how long this marker should be
  ;;@param marker-width how wide the top and bottom bars should be
  (define (dc-draw-v-dim-marker dim-str side-length marker-width)
    (lambda (dc dx dy)
      (let* ((south-side-y (+ dy side-length))
             (marker-east-x (+ dx marker-width))
             (half-marker-width (/ marker-width 2))
             (quarter-marker-width (/ half-marker-width 2))
             (half-marker-x (+ dx half-marker-width))
             (half-y (+ dy (/ side-length 2)))
             (half-text-height (/ (send dc get-char-height) 2));;just guessing text height for now
             (half-text-width (/ (* (string-length dim-str)(send dc get-char-width)) 2)));;just guessing text width for now
        (send dc draw-line dx dy marker-east-x dy);;north marker
        (send dc draw-line dx south-side-y marker-east-x south-side-y);;south marker
        (send dc draw-line half-marker-x dy half-marker-x (- half-y half-text-height))
        (send dc draw-text dim-str (- half-marker-x half-text-width) (- half-y half-text-height) #f 0 0)
        (send dc draw-line half-marker-x (+ half-y half-text-height) half-marker-x south-side-y)
        (send dc draw-line half-marker-x dy (+ half-marker-x quarter-marker-width)  (+ dy half-marker-width))
        (send dc draw-line half-marker-x south-side-y (+ dx quarter-marker-width) (- south-side-y half-marker-width)))))
  
   ;;draw a dimension marker like on a drafting picture 
  ;;(a set of two arrows: one pointing at and bracketed by a short vertical left line
  ;; and one pointing at and bracketed by a short vertical right line, both arrows pointing away from text
  ;; denoting the length of the dimension this dimension marker is marking.)
  ;;@param dim-str the text to be used for this marker to denote the length of the dimension this dimension marker
  ;; is marking.
  ;;@param side-length how long this marker should be
  ;;@param marker-width how wide the top and bottom bars should be
  (define (dc-draw-h-dim-marker dim-str side-length marker-width)
    (lambda (dc dx dy)
      (let* ((right-x (+ dx side-length))
             (half-side-length (/ side-length 2))
             (half-right-x (+ dx half-side-length))
             (half-bottom-y (+ dy (/ marker-width 2)))
             (half-marker-width (/ marker-width 2))
             (quarter-marker-width (/ half-marker-width 2))
             (half-text-height (/ (send dc get-char-height) 2));;just guessing text height for now
             (char-width (send dc get-char-width))
             (half-text-width (/ (* (string-length dim-str) char-width) 2)));;just guessing text width for now
        (send dc draw-line dx dy dx (+ dy marker-width));;left marker
        (send dc draw-line right-x dy right-x (+ dy marker-width));;right marker
        (send dc draw-line dx half-bottom-y (- half-right-x (+ half-text-width char-width))  half-bottom-y);;left arrow line
        (send dc draw-text dim-str (- half-right-x half-text-width) (- half-bottom-y half-text-height) #f 0 0) ;;text
        (send dc draw-line (+ half-right-x (+ half-text-width char-width)) half-bottom-y right-x half-bottom-y) ;;right arrow line
        (send dc draw-line dx half-bottom-y (+ dx half-marker-width) dy) ;;left arrow half-arrow
        (send dc draw-line right-x half-bottom-y (- right-x half-marker-width) (+ dy marker-width)))))
        
  (provide dc-draw-cross dc-draw-diamond dc-draw-square dc-draw-square-with-txt dc-draw-v-dim-marker dc-draw-h-dim-marker
           dc-draw-nw-half-sq-triangle dc-draw-ne-half-sq-triangle dc-draw-sw-half-sq-triangle dc-draw-se-half-sq-triangle))
