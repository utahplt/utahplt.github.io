#cs
(module lecture0 (lib "slideshow.ss" "slideshow")  

  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
	   "utils/lec0.ss"
	   (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (slide/center
   (vc-append
    line-sep
    (titlet "CS 2010")
    (titlet "Computer Science I"))
   (blank)
   (hbl-append
    (t "Instructor: ")
    (colorize (bt "Matthew Flatt") BlueColor)))
  
  (slide/title/center
   "This Course is About..."
   (page-para "Fundamentals of programming")
   (page-subitem "From specification to implementation")
   (page-subitem "Software engineering principles"))
  
  (slide/title/center
   "This Course is..."
   (page-para (bit "Not") "about...")
   (page-subitem "A particular programming language (e.g., Java, C++, Scheme)")
   (page-subitem "A particular programming tool (e.g., gcc, DrScheme)")
   (page-subitem "Specific libraries or protocols (e.g., Gtk, XML, HTTP)")
   (page-subitem "How programs get translated into electronic signals"))
  
  (slide/title/center
   "Book"
   (it "How to Design Programs")
   (bitmap "htdp-cover.gif"))
  
  (slide/title/center
   "Programming Environment"
   (it "DrScheme")
   (bitmap "drscheme.gif"))
  
  (slide/title/center
   "What is Scheme?"
   (page-item (bt "Scheme") "is a programming language")
   (page-subitem "Used to implement DrScheme, for example")
   (blank)
   (page-item "The language for this course matches a subset of Scheme")
   (page-item "The course content is not Scheme-specific"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/center
   (titlet "Getting Started:")
   (titlet "Arithmetic, Algebra, and Computing"))
  
  (slide/title
   "Arithmetic is Computing"
   (page-item "Fixed, pre-defined rules for" (dt "primitive operators") ":")
   'alts
   (list (list
          (page-para* "2 + 3 = 5")
          (page-para* "4" sym:times "2 = 8")
          (page-para* "cos(0) = 1"))
         (list
          (page-para* "2 + 3" rightarrow "5")
          (page-para* "4" sym:times "2" rightarrow "8")
          (page-para* "cos(0)" rightarrow "1")))
   'next
   (blank)
   (page-item "Rules for combining other rules:")
   (page-subitem "Evaluate sub-expressions first")
   (page-para* "4" sym:times "(2 + 3)" 
               rightarrow "4" sym:times "5" 
               rightarrow "20")
   'next
   (page-subitem "Precedence determines subexpressions:")
   (page-para* "4 + 2" sym:times "3" 
               rightarrow "4 + 6" 
               rightarrow "10"))

  (slide/title
   "Algebra as Computing"
   (page-subitem "Definition:")
   (page-para* "f(x) = cos(x) + 2")
   (page-subitem "Expression:")
   (page-para* "f(0)" rightarrow "cos(0) + 2" 
               rightarrow "1 + 2" 
               rightarrow "3")
   'next
   (blank)
   (page-item "First step uses the" (dt "substitution") "rule for functions"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/center
   "Notation"
   (page-item "Why do some primitive operators go in the middle, like +,"
              "while others go at the front, like cos?")
   (page-item "What are the precedence rules?")
   (page-item "How do we know which arguments go with which operators?")
   (page-item "Which parentheses are redundant?")
   (page-item "When does = mean definition and when does it mean a computation step?")
   (page-item "..."))
  
  (define (old/new-table l)
   (table 2
          (list* (colorize (bt "Old") BlueColor)
                 (colorize (bt "New") BlueColor)
                 l)
          cc-superimpose cc-superimpose
          (* 3 gap-size) gap-size))
  
  (slide/title/center
   "Simplified Expression Notation"
   (page-item "Put all operators at the front")
   (page-item "Start every operation with an open parenthesis")
   (page-item "Put a close parenthesis after the last argument")
   (page-item "Never add extra parentheses")
   (old/new-table (list (t "1 + 2") (code (+ 1 2))
                        (hbl-append (t "4 + 2 ") sym:times (t " 3")) (code (+ 4 (* 2 3)))
                        (t "cos(0) + 1") (code (+ (cos 0) 1)))))
          
  (slide/title
   "Simplified Definition Notation"
   (page-item "Use the keyword" (tt "define") "instead of =")
   (page-item "Put" (tt "define") "at the front, and group with parentheses")
   (page-item "Move open parenthesis from after function name to before")
   (old/new-table (list (t "f(x) = cos(x) + 2")
                        (code (define (f x) (+ (cos x) 2)))))
   'next
   (blank)
   (page-item "Move open parenthesis in function calls")
   (old/new-table (list (t "f(0)") (code (f 0))
                        (t "f(2+3)") (code (f (+ 2 3))))))
  
  (with-steps
   (orig subs inner final)
   (slide/title/center
    "Evaluation is the Same as Before"
    (vl-append
     line-sep
     (code (define (f x) (+ (cos x) 2)))
     (blank gap-size)
     (code (f 0))
     ((vafter subs) (page-para* rightarrow (code (+ (cos 0) 2))))
     ((vafter inner) (page-para* rightarrow (code (+ 1 2))))
     ((vafter final) (page-para* rightarrow (code 3))))))

  (slide/title
   "Beyond Numbers: Booleans"
   (page-para "Numbers are not the only kind of values:")
   (old/new-table
    (list (page-para* "1 < 2" rightarrow "true")
	  (page-para* (code (< 1 2)) rightarrow (code true))
	  (page-para* "1 > 2" rightarrow "true")
	  (page-para* (code (> 1 2)) rightarrow (code false))
	  (page-para* "1 > 2" rightarrow "true")
	  (page-para* (code (> 1 2)) rightarrow (code false))
	  (page-para* "2" (symbol 179) "2" rightarrow "true")
	  (page-para* (code (>= 1 2)) rightarrow (code true)))))

  (slide/title/center
   "Beyond Numbers: Booleans"
   (old/new-table
    (list (t "true and false")
	  (code (and true false))
	  (t "true or false")
	  (code (or true false))
	  (blank) (blank)
	  (t "1 < 2 and 2 > 3")
	  (code (and (< 1 2) (> 2 3)))
	  (page-para* "1" (symbol 163) "0 and 1 = 1")
	  (code (or (<= 1 0) (= 1 1)))
	  (page-para* "1" (symbol 185) "0")
	  (code (not (= 1 0))))))
  
  (slide/title/center
   "Beyond Numbers: Symbols"
   (page-para* (code (symbol=? 'apple 'apple)) rightarrow (code true))
   (page-para* (code (symbol=? 'apple 'banana)) rightarrow (code false)))

  
  (slide/title
   "Beyond Numbers: Images"
   (page-para* (code (filled-rect 35 35 'red)) rightarrow (box-pict 35 35 "red" #t))
   (page-para* (code (filled-circle 25 25 'blue)) rightarrow (box-pict 25 25 "blue" #f))
   'next
   (blank)
   (page-para* (code (image+ #,(box-pict 35 35 "red" #t) #,(box-pict 25 25 "blue" #f)))
               rightarrow rwb)
   'next
   (blank)
   (page-para* (code (offset-image+ #,(box-pict 35 35 "red" #t) 5 5 #,(box-pict 25 25 "blue" #f)))
               rightarrow (cc-superimpose
                           (box-pict 35 35 "red" #t)
                           (box-pict 25 25 "blue" #f)))
   'next
   (blank)
   image-steps)
  
  (define mf (code-align (bitmap "matthew.png")))
  
  (slide/title
   "Programming with Images"
   (page-para
    anonymize-defn)
   (blank)
   (page-para (code (anonymize #,mf))
              rightarrow "..." rightarrow
              (cc-superimpose mf (box-pict (pict-width mf) (pict-height mf) "blue" #f)))
   (blank)
   (colorize (it "Use the stepper to see all steps") RedColor))

  )
