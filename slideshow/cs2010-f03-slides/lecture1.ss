#cs
(module lecture1 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
	   "utils/lec0.ss"
	   "utils/lec1.ss"
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (slide/title 
   "Computation versus Programming"
   (page-item "Last time, we talked about computation")
   'next
   image-steps
   'next
   (blank)
   (page-item "Programming?")
   'next
   (hc-append
    (* 2 gap-size)
    (text "Write an anonymizer..." 'roman font-size)
    (colorize (arrow gap-size 0) GreenColor)
    (scale anonymize-defn 0.5 0.5))
   'next
   (blank)
   (page-para* "We somehow wrote the function in one big, creative chunk"))
  
  (slide/title
   "Programming"
   (page-para "Today:" (bit "How to Design Programs"))
   (blank)
   (page-item "Programming always requires creativity")
   (page-item "But a design rules can guide and focus creativity")
   'next
   (blank)
   (colorize
    (vl-append
     (t "Analogous to rules for composing music:" )
     (t "scales, chords, counterpoint, rhythms, etc."))
    BlueColor)
   'next
   (blank)
   (colorize
    (vc-append
     (t "Language syntax is like musical notation")
     (t "You need a notation, but notation alone gets you nowhere"))
    RedColor))

  (slide/title/center
   "The Design Recipe"
   (page-item "We'll start with a simple recipe")
   (page-item "As the course progresses, we'll expand the recipe"))
  
  (define (mk-recipe hilite)
    (if (and condense? hilite)
	(skip-slides 1)
	(design-recipe-I hilite)))
  
  (mk-recipe #f)
  (mk-recipe 'data)
  
  (define implies (inset (drop (colorize (arrow gap-size 0) BlueColor) gap-size) gap-size 0))
  
  (slide
   (recipe-item "Data")
   (page-para "Choose a representation suitable for the function input")
   (page-item "Fahrenheit degrees" implies (code num))
   (page-item "Grocery items" implies (code sym))
   (page-item "Faces" implies (code image))
   (page-item "Wages" implies (code num))
   (page-item "...")
   'next
   (blank)
   (page-para (colorize (t "Handin artifact:") RedColor) (bt "none") "for now"))
  
  (mk-recipe 'cph)
  
  (slide
   (recipe-item "Contract, Purpose, and Header")
   (page-para (dt "Contract"))
   (page-para "Describes input(s) and output data")
   (page-item (code f2c) (code :) (code num) (code ->) (code num))
   (page-item (code is-milk?) (code :) (code sym) (code ->) (code bool))
   (page-item (code wearing-glasses?) (code :) (code image) (code image) (code image) (code ->) (code bool))
   (page-item (code netpay) (code :) (code num) (code ->) (code num))
   'next
   (blank)
   (page-para (colorize (t "Handin artifact:") RedColor) "a comment")
   (vl-append
    (/ gap-size 2)
    (page-para* (code (code:contract f2c : num -> num)))
    (page-para* (code (code:contract is-milk? : sym -> bool)))))
  
  (define (rom x) (text x 'roman font-size))

  (slide
   (recipe-item "Contract, Purpose, and Header")
   (page-para (dt "Purpose"))
   (page-para "Describes, in English, what the function will do")
   (page-item (rom "Converts F-degrees") (code f) (rom "to C-degrees"))
   (page-item (rom "Checks whether") (code s) (rom "is a symbol for milk"))
   (page-item (rom "Checks whether") (code p2) (rom "is") (code p1) (rom "wearing glasses") (code g))
   (page-item (rom "Computes net pay (less taxes) for") (code n) (rom "hours worked"))
   'next
   (blank)
   (page-para (colorize (t "Handin artifact:") RedColor) "a comment after the contract")
   (vl-append
    line-sep
    (code (code:contract f2c : num -> num))
    (code (code:comment "Converts F-degrees f to C-degrees"))))

  (slide
   (recipe-item "Contract, Purpose, and Header")
   (page-para (dt "Header"))
   (page-para "Starts the function using variables that are metioned in purpose")
   (page-item (code (define (f2c f) ....)))
   (page-item (code (define (is-milk? s) ....)))
   (page-item (code (define (wearing-glasses? p1 p2 g) ....)))
   (page-item (code (define (netpay n) ....)))
   'next
   (page-para (colorize (t "Check:") GreenColor) "function name and variable count match contract")
   'next
   (page-para (colorize (t "Handin artifact:") RedColor) "as above, but absorbed into implementation")
   (vl-append
    line-sep
    (code (code:contract f2c : num -> num))
    (code (code:comment "Converts F-degrees f to C-degrees"))
    (code (define (f2c f) ....))))
   
  (mk-recipe 'examples)
  (slide
   (recipe-item "Examples")
   (page-para "Show example function calls an result")
   (vl-append
    line-sep
    (code (f2c 32) "should be" 0)
    (code (f2c 212) "should be" 100)
    (t "")
    (code (is-milk? 'milk) "should be" true)
    (code (is-milk? 'apple) "should be" false))
   'next
   (page-para (colorize (t "Check:") GreenColor) "function name, argument count and types match contract")
   'next
   (page-para (colorize (t "Handin artifact:") RedColor) "as above, after header/body")
   (vl-append
    line-sep
    (code (code:contract f2c : num -> num))
    (code (code:comment "Converts F-degrees f to C-degrees"))
    (code (define (f2c f) ....))
    (code (f2c 32) "should be" 0)
    (code (f2c 212) "should be" 100)))
   
  (mk-recipe 'body)
  (slide
   (recipe-item "Body")
   (page-para "Fill in the body under the header")
   (vl-append
    gap-size
    (code (define (f2c f)
            (* (- f 32) 5/9)))
    (code (define (is-milk? s)
            (symbol=? s 'milk))))
   'next
   (blank)
   (page-para (colorize (t "Handin artifact:") RedColor) "complete at this point")
   (vl-append
    line-sep
    (code (code:contract f2c : num -> num))
    (code (code:comment "Converts F-degrees f to C-degrees"))
    (code (define (f2c f)
            (* (- f 32) 5/9)))
    (code (f2c 32) "should be" 0)
    (code (f2c 212) "should be" 100)))
   
  (mk-recipe 'test)
  (slide
   (recipe-item "Test")
   (page-para "Click" (bt "Execute") "- examples serve as tests")
   (bitmap "test-step.png"))
                   
  (slide/title/center
   "Design Recipe - Each Step Has a Purpose"
   ;;
   (recipe-item "Data" "white")
   (recipe-desc "Shape of input data will drive the implementation")
   ;;
   (recipe-item "Contract, Purpose, and Header" "white")
   (recipe-desc "Provides a first-level understanding of the function")
   ;;
   (recipe-item "Examples" "white")
   (recipe-desc "Gives a deeper understanding and exposes specification issues")
   ;;
   (recipe-item "Body" "white")
   (recipe-desc "The implementation is the whole point")
   ;;
   (recipe-item "Test" "white")
   (recipe-desc "Evidence that it works"))
  
  
  (slide/title
   "Design Recipe FAQ"
   (page-item "Do I have to use the recipe when the function seems obvious?")
   (page-subitem (bt "Yes"))
   'next
   (page-item "Will my grade suffer if I don't handin recipe artifacts?")
   (page-subitem (bt "Yes"))
   'next
   (page-item "Isn't the recipe just a lot of obnoxious busy work?")
   (page-subitem (bt "No") sym:emdash "it's a training exercise")
   (colorize (vl-append
              (/ gap-size 2)
              (page-para* "As programs become more complex in the next few"
                          "weeks, the design recipe will prove more helpful")
              (page-para* "If you don't learn to use the recipe now, you'll be stuck having"
                          "to learn both the recipe and other concepts later on"))
             BlueColor))

  )

   
