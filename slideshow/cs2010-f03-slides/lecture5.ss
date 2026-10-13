#cs
(module lecture5 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
	   (lib "step.ss" "slideshow")
           (lib "face.ss" "texpict")
           (lib "math.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (slide/title
   "Data So Far"
   (page-item "Built-in atomic data:"
              (code num) "," (code bool) "," (code sym)
              ", and" (code image))
   (blank)
   (page-item "Built-in compound data:" (code posn))
   (blank)
   (page-item "Programmer-defined compound data:" (code define-struct)
              "plus a data definition")
   (blank)
   (page-item "Programmer-defined data with varieties:"
              "data definition with \"either\"")
   (blank)
   (page-para (colorize (t "Today:") BlueColor)
              "more examples"))
  
  (define face-yellow (make-object color% 240 220 0))
  (define face-green (make-object color% 200 250 0))
  
  (define (sface mood)
    (scale (case mood
             [(ill)
              (face* 'worried 'tongue #t face-green 6 0 0 -8)]
             [(unhappy)
              (face* 'none (if printing? 'plain 'medium) #t face-yellow 3)]
             [(happier)
              (face* 'none (if printing? 'plain 'large) #f face-yellow 3)])
           0.5 0.5))
  
  (define (make-grade v)
    (cc-superimpose (file-icon (* gap-size 3) (* gap-size 4) #t)
                    (colorize (tt (number->string v)) RedColor)))
  
  (define (make-graded who grades)
    (vc-append (/ gap-size 3)
               who
               (apply hc-append
                      (/ gap-size 3)
                      (map (lambda (i)
                             (if (number? i)
                                 (make-grade i)
                                 i))
                           grades))))
  
  (define rx
    (cc-superimpose
     (ghost (make-grade 0))
     (file-icon (* gap-size 2)
                (* gap-size 8/3)
                (make-object color% 255 255 150))
     (let ([x (t "x")])
       (hbl-append (- (* (pict-width x) 1/3))
                   (t "R")
                   (drop x (* (pict-height x) 1/3))))))
  
  (with-steps~
   (stmt good bad sick rest)
   (slide/title
    "Example 1: Managing Grades"
    (page-para "Suppose that we need to manage exam grades")
    (hc-append
     (* 3 gap-size)
     ((vafter good) (make-graded (sface 'happier) (list 100)))
     ((vafter bad) (make-graded (sface 'unhappy) (list 0)))
     ((vafter sick) (make-graded (sface 'ill) (list rx))))
    (blank)
    ((vafter rest)
     (vc-append
      gap-size
      (page-item "Record a grade for each student")
      (page-item "Distinguish zero grade from missing the exam")
      (blank)
      (page-para "We want to implement" (code passed-exam?))))))

  (define grade-defn
    (code (code:comment "A grade is either")
          (code:comment " - num")
          (code:comment " - empty")))
  
  (slide/title/tall
   "Programming with Grades"
   (recipe-item "Data")
   'alts
   (list (list
          (page-item "Use a number for a grade, obviously")
          'next
          (page-item "For a non-grade, use the built-in constant" (code empty))
          (blank)
          (blank)
          (page-para (code empty) "is something that you can use to represent nothing.")
          (page-para "It's not a" (code num) "," (code bool) "," (code sym)
                     "," (code image) ", or" (code posn) "."))
         (list grade-defn
               'next
               (blank)
               (blank)
               (page-para (colorize (t "Examples:") BlueColor))
               (code 100)
               (code 0)
               (code empty))))
  
  (with-steps
   (contract purpose header examples template2 body)
   (slide/title/tall
    "Programming with Grades"
    (cc-superimpose
     ((vbetween contract header) (recipe-item "Contract, Purpose, and Header"))
     ((vbetween examples examples) (recipe-item "Examples"))
     ((vbetween template2 template2) (recipe-item "Template"))
     ((vafter body) (recipe-item "Body")))
    (vl-append
     line-sep
     (code (code:contract passed-exam? : grade -> bool))
     ((vafter purpose) (code (code:comment "Determines whether g is 70 or better")))
     (lt-superimpose
      ((vbetween header examples) (code (define (passed-exam? g)
                                            ...)))
      ((vbetween template2 template2) 
       (vc-append
        line-sep
        (code (define (passed-exam? g)
                (cond
                  [(number? g) ...]
                  [(empty? g) ...])))
        (tt " ")
        (colorize (page-para* "varieties" sym:implies (code cond))
                  RedColor)))
      ((vafter body) 
       (code (code:template (define (passed-exam? g)
                              (cond
                                [(number? g) ...]
                                [(empty? g) ...])))
             (define (passed-exam? g)
               (cond
                 [(number? g) (>= g 70)]
                 [(empty? g) false])))))
     (tt " ")
     ((vafter examples)
      (vl-append
       line-sep
       (code (passed-exam? 100) "should be" true)
       (code (passed-exam? 0) "should be" false)
       (code (passed-exam? empty) "should be" false))))))

  (define regrade-defn
    (code (code:comment "A grade is either")
          (code:comment " - num")
          (code:comment " - posn")
          (code:comment " - empty")))

  (slide/title
   "Grades and Re-takes"
   (page-para "Suppose that we allow one re-test per student")
   (hc-append
    (* 3 gap-size)
    (make-graded (sface 'happier) (list 100))
    (make-graded (sface 'unhappy) (list 0 80))
    (make-graded (sface 'ill) (list rx)))
   'next
   (blank)
   regrade-defn)
  
  (with-steps
   (cph examples template template2)
   (slide/title/tall
    "Programming with Grades and Retests"
    (cc-superimpose
     ((vbetween cph cph) (recipe-item "Contract, Purpose, and Header"))
     ((vbetween examples examples) (recipe-item "Examples"))
     ((vbetween template template2) (recipe-item "Template")))
    (vl-append
     line-sep
     (code (code:contract passed-exam? : grade -> bool))
     ((vafter cph) (code (code:comment "Determines whether g is 70 or better")))
     (lt-superimpose
      ((vbetween-excl cph template) (code (define (passed-exam? g)
					    ...)))
      ((vbetween template template) 
       (vc-append
        line-sep
        (code (define (passed-exam? g)
                (cond
                  [(number? g) ...]
                  [(posn? g) ...]
                  [(empty? g) ...])))
        (tt " ")
        (colorize (page-para* "varieties" sym:implies (code cond))
                  RedColor)))
      ((vbetween template2 template2) 
       (vc-append
        line-sep
        (code (define (passed-exam? g)
                (cond
                  [(number? g) ...]
                  [(posn? g) ... (posn-passed-exam? g) ...]
                  [(empty? g) ...])))
        (tt " ")
        (colorize (page-para* "data-defn reference" sym:implies "template reference")
                  RedColor))))
     (tt " ")
     ((vafter examples)
      (vl-append
       line-sep
       (code (passed-exam? 100) "should be" true)
       (code (passed-exam? (make-posn 0 80)) "should" true)
       (code (passed-exam? empty) "should be" false))))))

  (slide/title
   "Complete Function"
   (code (code:contract passed-exam? : grade -> bool)
         (define (passed-exam? g)
           (cond
             [(number? g) (>= g 70)]
             [(posn? g) (posn-passed-exam? g)]
             [(empty? g) false]))
         code:blank
         (code:contract posn-passed-exam? : posn -> bool)
         (define (posn-passed-exam? p)
           (or (>= (posn-x p) 70)
               (>= (posn-y p) 70))))
   (blank)
   (colorize (page-para (it "Plus tests and templates..."))
             BlueColor))
        
  (define regrade-tmpl
    (code (define (func-for-grade g)
            (cond
              [(number? g) ...]
              [(posn? g) ... (func-for-posn g) ...]
              [(empty? g) ...]))))
  
  (slide/title
   "Shapes of Data and Functions"
   (page-para "As always, the shape of the function matches the shape of the data")
   (blank)
   (page-para
    (scale
     (add-gp-arrow
      (vl-append
       line-sep
       regrade-defn
       (tt " ")
       (code (code:comment "A posn is"))
       (code (code:comment " (make-posn num num)")))
      1/3 3/7 1/3 5/7)
      0.75 0.75))
   (blank)
   (page-para/r
    (scale
     (add-gp-arrow
      (vl-append
       line-sep
       regrade-tmpl
       (tt " ")
       (code (define (func-for-posn p)
               ... (posn-x p) ... (posn-y p) ..)))
      2/3 4/8 1/2 6/8)
     0.75 0.75)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define steven (code-align (bitmap "steven-moore.png")))
  (define adam (code-align (bitmap "adam-thompson.png")))
  
  (with-steps~
   (appt prog)
   (slide/title
    "Example #2: Day Planning"
    (page-para "Suppose that we need to manage day-planner entries")
    (let* ([items (list steven (t "@lab")
                        (blank) (blank)
                        adam (t "@office")
                        (blank) (blank))]
           [block (inset
                   (blank (apply max (map pict-width items))
                          (apply max (map pict-height items)))
                   line-sep)]
           [items (map (lambda (i) (cc-superimpose block i)) items)])
      (hc-append
       (* 2 gap-size)
       (let loop ([items items])
         (if (null? items)
             (blank)
             (vc-append (frame (hc-append (car items) (cadr items)))
                        (loop (cddr items)))))
       (vl-append
        gap-size
        (para (/ client-w 2)
              "Each day-plan is either empty"
              "or an appointment with person and place")
        (blank)
        ((vafter prog)
         (para (/ client-w 2)
               "Implement" (code close-blinds?)))
        (blank)
        ((vafter prog)
         (colorize
          (para (/ client-w 2)
                "for Adam's sensitive eyes during office meetings")
          BlueColor)))))))

  (define dp-defn
    (code (code:comment "An day-plan is either")
          (code:comment " - empty")
          (code:comment " - (make-appt image sym)")))

  (define dp-tmpl-code
    (code (define (close-blinds? dp)
            (cond
              [(empty? dp) ...]
              [(appt? dp)
               ... (appt-who dp)
               ... (appt-where dp) ...]))))
  
  (slide/title/tall
   "Programming with Day-Plans"
   (recipe-item "Data")
   (vl-append
    line-sep
    dp-defn
    (code (define-struct appt (who where))))
   'next
   (blank)
   (colorize (page-para "Examples:") BlueColor)
   (code empty)
   (blank)
   (code (make-appt #,adam 'office)))

  (with-steps
   (contract purpose header examples template template1 template2 body)
   (slide/title/tall
    "Programming with Day-Plans"
    (cc-superimpose
     ((vbetween contract header) (recipe-item "Contract, Purpose, and Header"))
     ((vbetween examples examples) (recipe-item "Examples"))
     ((vbetween template template2) (recipe-item "Template"))
     ((vafter body) (recipe-item "Body")))
    (cb-superimpose
     (vl-append
      line-sep
      (code (code:contract close-blinds? : day-plan -> bool))
      ((vafter purpose) (code (code:comment "Determines whether dp is a meeting")
			       (code:comment "with Adam at office")))
      (lt-superimpose
       ((vbetween header template) 
	(vl-append
	 line-sep
	 (code (define (close-blinds? dp)
		 ...))
	 ((vbetween examples examples)
	  (vl-append
	   line-sep
	   (code (close-blinds? empty) "should be" false)
	   (code (close-blinds? (make-appt #,adam 'office)))
	   (code "should be" true)
	   (code (close-blinds? (make-appt #,steven 'lab)))
	   (code "should be" false)))))
       ((vbetween template1 template1) 
	(vc-append
	 line-sep
	 (code (define (close-blinds? dp)
		 (cond
                  [(empty? dp) ...]
                  [(appt? dp) ...])))
	 (tt " ")
	 (colorize (page-para* "varieties" sym:implies (code cond))
		   RedColor)))
       ((vbetween template2 template2) 
	(vc-append
	 line-sep
	 dp-tmpl-code
	 (tt " ")
	 (colorize (page-para* "compound data" sym:implies "extract parts")
		   RedColor)))
       ((vafter body) 
	(code (define (close-blinds? dp)
		(cond
                 [(empty? dp) false]
                 [(appt? dp)
                  (and
                   (image=? (appt-who dp) #,adam)
                   (symbol=? (appt-where dp) 'office))]))))))
     ((vbetween template template2) dp-defn))))

  (slide/title/center
   "Shapes of Data and Functions"
   (page-para "As always, the shape of the function matches the shape of the data")
   (blank)
   (page-para dp-defn)
   (blank)
   (blank)
   (page-para/r dp-tmpl-code))
  
  (slide/title/center
   "Summary"
   (page-para "Today's examples show:")
   (page-item "A data definition with variants need not involve structure choices")
   (page-item "A data definition with variants can include" 
              (hbl-append (code make-) (it "something")) "directly")
   (page-para/r "... usually when the structure by itself isn't useful")
   (page-item "Implementation shape still matches the data shape")
   (blank)
   (colorize (page-para "No recipe changes!") BlueColor))
  
  )



