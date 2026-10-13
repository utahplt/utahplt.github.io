#cs
(module lecture16 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
	   "utils/utils.ss"
           "utils/code.ss"
           "utils/eval-step.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss"))
  
  (define plt-icon
    (bitmap (build-path (collection-path "icons") "PLTnolarval.jpg")))
  
  (slide/center
   plt-icon)
  
  (slide/title
   "Values and Names"
   (page-para "Some Values:")
   (page-item "Numbers:" (code 1) "," (code 17.8) "," (code 4/5))
   (page-item "Booleans:" (code true) "," (code false))
   (page-item "Lists:" (code empty) "," (code (cons 7 empty)))
   (page-item "...")
   (vc-append
    line-sep
    (page-item "Function" (it "names") ":"
               (code less-than-5) "," (code first-is-apple?))
    (para (* 2/3 client-w) "given" (code (define (less-than-5? n) ...)
                                         (define (first-is-apple? a b) ...))))
   'next
   (blank)
   (colorize (page-para* "Why do only function values require names?") BlueColor))
  
  (slide/title
   "Naming Everything"
   (page-para "Having to name" (it "every") "kind of value would be painful:")
   (vl-append
    gap-size
    (code
     (local [(define (first-is-apple? a b)
               (symbol=? a 'apple))]
       (choose '(apple banana) '(cherry cherry) 
               first-is-apple?)))
    (page-para* "whould have to be")
    (code
     (local [(define (first-is-apple? a b)
               (symbol=? a 'apple))
             (define al '(apple banana))
             (define bl '(cherry cherry))]
       (choose al bl first-is-apple?))))
   (blank)
   (page-para "Fortunately, we don't have to name lists"))
  
  (slide/title
   "Naming Nothing"
   (page-para "Can we avoid naming functions?")
   (page-para "In other words, instead of writing")
   (code (local [(define (first-is-apple? a b)
                   (symbol=? a 'apple))]
           ... first-is-apple? ...))
   (page-para "we'd like to write")
   (code ... 
         code:blank #,(page-para* "function that takes" (code a) "and" (code b))
         code:blank code:blank #,(page-para* "and produces" (code (symbol=? a 'apple)))
         ...)
   'next
   (blank)
   (colorize
    (page-para* "We can do this in" (bt "Intermediate with Lambda"))
    BlueColor))
  
  (slide/title
   "Lambda"
   (page-para "An" (dt "anonymous function") "value:")
   (code (lambda (a b) (symbol=? a 'apple)))
   (blank)
   (page-para "Using" (code lambda) "the original example becomes")
   (code (choose '(apple banana) '(cherry cherry) 
                 (lambda (a b) (symbol=? a 'apple))))
   'next
   (blank)
   (blank)
   (colorize
    (vc-append
     gap-size
     (page-para* "Why the funny keyword" (code lambda) "?")
     (page-para* "It's a 70-year-old convention: the Greek letter" 
                 (symbol (char->integer #\l)) "means \"function\""))
    BlueColor)
   (scale plt-icon 0.5))
  
  (slide/title
   "Using Lambda"
   (page-para "In DrScheme:")
   'alts
   (list (list
          (code > (lambda (x) (+ x 10))
                (lambda (a1) ...))
          (page-para "Unlike most kinds of values, there's no one shortest name:")
          (page-item "The argument name is arbitrary")
          (page-item "The body can be implemented in many different ways")
          (page-para "So DrScheme gives up" sym:emdash "it invents argument names and hides the body"))
         (list
          (code > ((lambda (x) (+ x 10)) 17)
                27)
          (page-para "The function position of an" (dt "application") 
                     "(i.e., function call) is no longer always"
                     "an identifier")
          'next
          (blank)
          (page-para "Some former syntax errors are now run-time errors:")
          (vl-append
           line-sep
           (code > (2 3))
           (colorize (it "procedure application: expected procedure, given 2") RedColor)))))

  (define long-form
    (code (define f (lambda (a b) 
                      (+ a b)))))

  (slide/title
   "Defining Functions"
   (page-para "What's the difference between")
   (lt-superimpose
    (code (define (f a b)
            (+ a b)))
    (ghost long-form))
   (page-para "and")
   long-form
   (page-para "?")
   'next
   (blank)
   (page-para "Nothing" sym:emdash "the first one is (now) a shorthand for the second"))
  
  (slide/title
   "Lambda and Built-In Functions"
   (page-para "Anonymous functions work great with" (code filter) "," (code map) ", etc.:")
   (code (define (eat-apples l)
           (filter (lambda (a) 
                     (not (symbol=? a 'apple))) 
                   l))
         code:blank
         (define (inflate-by-4% l)
           (map (lambda (n) (* n 1.04)) l))
         code:blank
         (define (total-blue l)
           (foldr (lambda (c n)
                    (+ (color-blue c) n))
                  0 l))))
  
  (slide/title
   "Functions that Produce Functions"
   (page-para "We already have functions that take function arguments")
   (code map : (X -> Y) list-of-X -> list-of-Y)
   (page-para "How about functions that" (it "produce") "functions?")
   'next
   (blank)
   (page-para "Here's one:")
   (code (code:contract make-adder : num -> (num -> num))
         (define (make-adder n)
           (lambda (m) (+ m n)))
         code:blank
         (map (make-adder 10) '(1 2 3))
         (map (make-adder 11) '(1 2 3))))

  (slide/title
   "Using Functions that Produce Functions"
   (page-para "Suppose that we need to filter different symbols:")
   (code (filter (lambda (a) (symbol=? a 'apple)) l)
	 (filter (lambda (a) (symbol=? a 'banana)) l)
	 (filter (lambda (a) (symbol=? a 'cherry)) l))
   'next
   (page-para "Instead of repeating the long" (code lambda) "expression, we can abstract:")
   (code (code:contract mk-is-sym : sym -> (sym -> bool))
	 (define (mk-is-sym s)
	   (lambda (a) (symbol=? s a)))
	 code:blank
	 (filter (mk-is-sym 'apple) l)
	 (filter (mk-is-sym 'banana) l)
	 (filter (mk-is-sym 'cherry) l))
   'next
   (blank)
   (colorize
    (page-para* (code mk-is-sym) "is a" (dt "curried") "version of" (code symbol=?))
    BlueColor))

  (define curry-defn
    (code (code:contract curry : (X Y -> Z) -> (X -> (Y -> Z)))
	  (define (curry f)
	    (lambda (v1)
	      (lambda (v2)
		(f v1 v2))))))

  (slide/title
   "! Currying Functions !"
   (page-para "This" (code curry) "function curries any 2-argument function:")
   'alts
   (list (list
	  (vl-append
	   line-sep
	   curry-defn
	   (code
	    code:blank
	    (define mk-is-sym (curry symbol=?))
	    code:blank
	    (filter (mk-is-sym 'apple) l)
	    (filter (mk-is-sym 'banana) l)
	    (filter (mk-is-sym 'cherry) l))))
	 (list
	  (vl-append
	   line-sep
	   curry-defn
	   (code
	    code:blank
	    (filter ((curry symbol=?) 'apple) l)
	    (filter ((curry symbol=?) 'banana) l)
	    (filter ((curry symbol=?) 'cherry) l))))))

  (slide/title
   "! Composing Functions !"
   (page-para "But we want" (hbl-append (it "non-") (t "symbols")))
   (code (code:contract compose (Y -> Z) (X ->Y) -> (X -> Z))
	 (define (compose f g)
	   (lambda (x) (f (g x))))
	 code:blank
	 (filter (compose 
		  not 
		  ((curry symbol=?) 'apple))
		 l)))

  (slide/title
   "! Uncurrying Functions !"
   (page-para "Sometimes it makes sense to" (dt "uncurry") ":")
   (code (code:contract curry : (X -> (Y -> Z)) -> (X Y -> Z))
	 (define (uncurry f)
	    (lambda (v1 v2)
	      ((f v1) v2)))
	 code:blank
	 (define (map f l)
	   (foldr (uncurry (compose (curry cons) f)) 
		  empty l))
	 code:blank
	 (define (total-blue l)
	   (foldr (uncurry (compose (curry +)
				    color-blue))
		  0 l))))

  (slide/title
   "Lambda in Math"
   (code (code:contract derivative : (num -> num) -> (num -> num))
         (define (derivative f)
           (lambda (x)
             (/ (- (f (+ x delta))
                   (f (- x delta)))
                (* 2 delta))))
         (define delta 0.0001)
         code:blank
         (define (square n) (* n n))
         ((derivative square) 10))
   (page-para "Produces roughly" (code 20) ", because the derivative of"
              (hbl-append (it "x") (text "2" `(superscript . ,(current-main-font))
                                         (current-font-size)))
              "is" (hbl-append (t "2") (it "x"))))
   
  (slide/title
   "Lambda in Real Life"
   (page-para "Graphical User Interfaces (GUIs) often use functions"
              "as values, including anonymous functions")
   (colorize (page-para/r (it "Java equivalent: inner classes")) RedColor)
   (blank)
   (blank)
   (scale (bitmap "greet-gui.jpg") 2)
   (colorize
    (page-para* "Button click" sym:implies "update bottom text")
    BlueColor))

  (slide/title
   "GUI Library"
   (scale/improve-new-text
    (vl-append
     gap-size
     (page-para (code make-text : string -> gui-item))
     (page-para (code text-contents : gui-item -> string))
     (blank)
     (blank)
     (page-para (code make-message : string -> gui-item))
     (page-para (code draw-message : gui-item string -> bool))
     (blank)
     (blank)
     (page-para (code make-button : string (event -> bool) -> gui-item))
     (blank)
     (blank)
     (page-para (code create-window : list-of-list-of-gui-item -> bool)))
    0.9 0.9))
     
  (slide/title
   "GUI Example"
   (page-para
    (scale/improve-new-text
     (code
      (define (greet what)
        (draw-message greet-msg
                      (string-append
                       what ", "
                       (text-contents name-field))))
      code:blank
      (define name-field 
        (make-text "Name:"))
      (define hi-button
        (make-button "Hello" (lambda (evt) (greet "Hi"))))
      (define bye-button
        (make-button "Goodbye" (lambda (evt) (greet "Bye"))))
      (define greet-msg
        (make-message "__________________________")))
     0.85)))
  
  (slide/title
   "GUI Example Improved"
   (page-para
    (scale/improve-new-text
     (code
      (define (mk-greet what)
        (lambda (evt)
          (draw-message greet-msg
                        (string-append
                         what ", "
                         (text-contents name-field)))))
      (define name-field
        (make-text "Name:"))
      (define hi-button
        (make-button "Hello" (mk-greet "Hi")))
      (define bye-button
        (make-button "Goodbye" (mk-greet "Bye")))
      (define greet-msg
        (make-message "__________________________")))
     0.85)))
  
  )

