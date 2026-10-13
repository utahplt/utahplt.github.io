#cs
(module lecture8 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/recipe.ss"
           (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (slide/title
   "More List-of-Num Examples"
   (code (code:comment "A list-of-num is either")
	 (code:comment " - empty")
	 (code:comment " - (cons num list-of-num)"))
   (blank)
   (problem "Implement the function" (code feed-fish)
	    ", which takes an aquarium and feeds each fish 1 lb of food")
   (problem "Implement the function" (code large-fish)
	    ", which removes every fish that is less than 5 lbs from an aquarium"))

  (define lop-defn
    (code (code:comment "A list-of-posn is either")
	  (code:comment " - empty")
	  (code:comment " - (cons posn list-of-posn)")))

  (define gp-lop-defn
    (add-gp-arrow lop-defn 3/4 23/30 1/3 8/30))

  (define together-defn
    (add-gp-arrow
     (vl-append
      line-sep
      gp-lop-defn
      (tt " ")
      (code (code:comment "A posn is")
			(code:comment " (make-posn num num)")))
     1/2 28/60 1/4 43/60))
  
  (define together-tmpl
    (code (code:contract func-for-lop : list-of-posn -> ...)
          (define (func-for-lop l)
            (cond
              [(empty? l) ...]
              [(cons? l)
               ... (func-for-posn (first l))
               ... (func-for-lop (rest l)) ...]))
          code:blank
          (code:contract func-for-posn : posn -> ...)
          (define (func-for-posn p)
            ... (posn-x p) ... (posn-y p) ...)))
  
  (define gp-together-tmpl
    (add-gp-arrow
     (add-gp-arrow
      together-tmpl
      1/2 52/110 1/3 18/110)
     1/2 68/110 1/2 92/110))
    
                                      
  (slide/title
   "List-of-Posn"
   'alts (list 
	  (list lop-defn)
	  (list gp-lop-defn)
	  (list together-defn)
          (list (scale together-defn 0.6 0.6)))
   (blank)
   'alts
   (list (list
          (code (code:contract func-for-lop : list-of-posn -> ...)
                (define (func-for-lop l)
                  (cond
                    [(empty? l) ...]
                    [(cons? l) ...]))))
         (list
          (code (code:contract func-for-lop : list-of-posn -> ...)
                (define (func-for-lop l)
                  (cond
                    [(empty? l) ...]
                    [(cons? l)
                     ... (first l)
                     ... (rest l) ...]))))
         (list
          (code (code:contract func-for-lop : list-of-posn -> ...)
                (define (func-for-lop l)
                  (cond
                    [(empty? l) ...]
                    [(cons? l)
                     ... (first l)
                     ... (func-for-lop (rest l)) ...]))))
         (list
          together-tmpl)
         (list
          gp-together-tmpl)))

  (slide/title
   "List-of-Posn Examples"
   (problem "Implement the function" (code flip-posns)
	    ", which flips the X and Y parts of every posn in a list of posns"))
  
  (slide/title
   "List-of-Grade Example"
   (code (code:comment "A grade is either")
         (code:comment " - number")
         (code:comment " - empty"))
   (blank)
   (problem "Implement the function" (code all-passed?)
	    ", which takes a list of grades and determines whether all are passes"))
  
  (slide/title
   "List-of-List-of-Num Example"
   (code (code:comment "A list-of-lon is either")
         (code:comment " - empty")
         (code:comment " - (cons list-of-num list-of-lon)"))
   (blank)
   (problem "Implement the function" (code sums)
	    ", which takes a list of list-of-numbers"
            "and produces a list of sums"))

  (slide/title
   "Writing Down Large Lists"
   (page-para "What does the list containing 0 to 10 look like?")
   'next
   (scale
    (code (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 empty))))))))))))
    0.4 0.4)
   'next 
   (blank)
   (blank)
   (page-para "Here's a shortcut:")
   (code (list 0 1 2 3 4 5 6 7 8 9 10))
   (blank)
   (page-para "The" (code list) "operator takes any number of arguments and constructs a list")
   'next
   (blank)
   (colorize (page-para* "Still, DrScheme prints 11" (hbl-append (code cons) (t "es")))
             BlueColor))
  
  (slide/title
   "Printing Large Lists"
   (vc-append
    line-sep
    (page-para "If you change DrScheme's language level to")
    (bt "Beginning Student with List Abbreviations")
    (page-para "then DrScheme prints using the shortcut"))
   (blank)
   (para (* 3/4 client-w)
         (code > (list 0 1 2 3 4 5 6 7 8 9 10)
               (list 0 1 2 3 4 5 6 7 8 9 10)))
   'next
   (para (* 3/4 client-w)
         (code > (cons 1 (cons 2 (cons 3 empty)))
               (list 1 2 3))))
  
  (slide/title
   "When to Change Language Levels"
   (vc-append
    line-sep
    (page-para "1. You're not tempted to write examples like this:")
    (code (feed-fish (cons 1 (cons 2 empty)))
          "should be" 2 3))
   'next
   (blank)
   (vc-append
    line-sep
    (page-para "2. Your eyes hurt when you see")
    (code (cons 1 (cons 2)))
    (page-para "because it isn't a" (code list-of-num)))
   'next
   (blank)
   (vc-append
    line-sep
    (page-para "3. When you see")
    (code (list 1 2 3))
    (code (cons 1 (cons 2 (cons 3 empty))))
    (page-para "you recognize instantly that they're the same"))
   'next
   (blank)
   (colorize
    (page-para*/c "Don't switch until you understand how" (code list-of-...)
                  "functions match the shape of the data definition")
    BlueColor))
  
  (slide/title
   "Even Shorter"
   (page-para "For the brave, there's an even shorter shortcut!")
   'next
   (vc-append
    line-sep
    (code '(1 2 3))
    (page-para* "is the same as")
    (code (list 1 2 3)))
   (page-para "The apostrophe above doesn't make a symbol" (symbol 190)
              "it makes a list because it precedes a parenthesis")
   'next
   (blank)
   (page-para "Furthermore, the apostrophe gets distributed to everything inside:")
   (vc-append
    line-sep
    (code '(apple banana))
    (page-para* "is the same as")
    (code (list 'apple 'banana)))
   'next
   (blank)
   (page-para "For consistency," (code '1) "is the same as" (code 1)))
  
  (slide/title
   "Even Shorter"
   (page-para "Here's a" (code list-of-lon) "using the shortcut:")
   (code '((1 2 3) (2 4 6 8) (3 9 27)))
   'next
   (page-para "which is the same as")
   (scale (code (list (list 1 2 3) (list 2 4 6 8) (list 3 9 27))) 0.8 0.8)
   'next
   (page-para "which is the same as")
   (scale (code (cons (cons 1 (cons 2 (cons 3 empty)))
                      (cons (cons 2 (cons 4 (cons 6 (cons 8 empty))))
                            (cons (cons 3 (cons 9 (cons 27 empty)))
                                  empty)))) 0.8 0.8))
   
  )
