
(module lecture6 (lib "slideshow-run.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss"
           (lib "etc.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title 
   "Today"
   (page-para "1. Add top-level function defines to the Book language")
   (page-subitem "not in the book")
   (blank)
   (colorize (page-para "Before we implement local functions...") BlueColor)
   (page-para "2. How to design better programs with local functions")
   (page-subitem "also not in the book, but in" (it "HtDP")))

  (slide/title
   "Top-Level Procedure Definitions"
   (page-para "Concrete syntax:")
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "{{ id funcdef }} ** in expr") (blank)
     (alg-code "funcdef") eqls (alg-code "(id**) = expr") (blank)
     (alg-code "expr")    eqls (alg-code "(id expr**)") (blank)))
   'next
   (blank)
   'alts
   (list (list (alg-code* "identity(x) = x"
			  "in (identity 7)"))
	 (list (alg-code* "fact(n) = if n then *(n, (fact -(n, 1))) else 1"
			  "identity(x) = x"
			  "in (identity (fact 10))"))))

  (slide/title
   "Top-Level Procedure Definitions"
   (page-para "Abstract syntax:")
   (grammar-table
    (list
     (alg-code "prog")    eqls (alg-code "(a-program ") (blank)
     (blank)              (blank) (alg-code "   (list id**) (list funcdef**) expr)") (blank)
     (alg-code "funcdef") eqls (alg-code "(a-funcdef (list id**) expr)") (blank)
     (alg-code "expr")    eqls (alg-code "(app-exp id (list expr**))") (blank)))
   'next
   (page-item "When evaluating a procedure application, we'll need a way to find"
	      "a defined procedure")
   'next
   (page-subitem "Use an environment (so we have two: local and top-level)"))

  (slide/title
   "Implementing Top-Level Procedure Definitions"
   (page-para* "(implement in DrScheme)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "How to Design Better Programs"
   (page-para "Let's open an aquarium")
   (page-item "At first, we only care about the weight of each fish")
   (page-item "Represent a fish as a number")
   (page-item "Represent the aquarium as a list of numbers")
   (page-item "Functions include" (alg-code "big") ", which takes an"
              "aquarium and returns only the fish bigger than 5 pounds"))
  
  (slide/title
   "Aquarium Functions"
   (page-para "Start with a template (generic):")
   (alg-code*
    ";; lon-function : list-of-num -> ????"
    "(define (lon-function l)"
    "  (cond"
    "   [(null? l) ...]"
    "   [(pair? l) ... (car l) ... (lon-function (cdr l)) ...]))"))
  
  (define size-func
    (opt-lambda (name comparator [comp-arg ""] [comp-type ""] [five " 5"])
      (alg-code*
       (format ";; ~a :~a l-o-n -> l-o-n" name comp-type)
       (format "(define (~a~a l)" name comp-arg)
       "  (cond"
       "    [(null? l) '()]"
       "    [(pair? l) "
       "     (cond"
       (format "          [(~a (car l)~a) (cons (car l) (~a~a (cdr l)))]"
               comparator five name comp-arg)
       (format "          [else (~a~a (cdr l))])]))" name comp-arg))))

  (slide/title
   "Getting the Big Fish"
   (size-func "big" ">")
   'next
   (blank)
   (alg-code "(big '(2 4 10)) -> '(10)"))

  (slide/title
   "Getting the Small Fish"
   'alts~
   (list (list (size-func "small" "<")
               (blank)
               (alg-code "(small '(2 4 10)) -> '(2 4)"))
         (list (size-func "@small" "@<")
               (blank)
               (page-item "Tiny changes to" (alg-code "big") ", so cut-and-paste old code?"))))
  
  (slide/title
   "A Note on Cut and Paste"
   (blank)
   (page-para* "When you cut and paste code, you cut and paste bugs")
   (blank)
   (page-para* (colorize (bit "Avoid cut-and-paste whenever possible!") BlueColor))
   (blank)
   (blank)
   'next
   (page-item "Alternative to cut and paste:" (dt "abstraction")))
  
  (define ffilter "filter-fish")
  
  (slide/title
   "Filtering Fish"
   'alts~
   (list (list (page-para (size-func ffilter "@OP" " @OP" " ????")))
         (list (page-para (size-func ffilter "@OP" " @OP" " (num num -> bool)"))
               'next
               (page-para (alg-code* (format "(define (big l) (~a > l))" ffilter)))
               (page-para (alg-code* (format "(define (small l) (~a < l))" ffilter))))))
  
  (slide/title
   "More Filters"
   'alts
   (list (list
          (page-item "Medium fish?")
          'next
          (colorize (page-para "No problem:") BlueColor)
          (alg-code* (format "(define (medium l) (~a = l))" ffilter)))
         (list 
          (page-item "How about fish that are" (it "roughly") "medium, between 4 and 6 pounds?")
          'next
          (alg-code* "close-to : num num -> bool"
                     "(define (close-to n m)"
                     "  (and (>= n (- m 1)) (<= n (+ m 1)))"
                     " "
                     (format "(define (roughly-medium l) (~a close-to l))" ffilter))
          'next
          (blank)
          (colorize (page-para* "Remember:" (bt "function names are values!")) RedColor)
          (blank)
          (colorize (page-para* (it "Note the contract for") (alg-code "close-to")) BlueColor))
         (list
          (page-item "How about 2-pound fish?")
          'next
          (colorize (page-para "Abstract" (alg-code ffilter) "with respect to the number 5?") BlueColor)
          'alts
          (list (list (size-func ffilter "OP" " OP N" " ... num" " N"))
                (list
                 (page-item "How about fish that are either 2 pounds or 4 pounds?")))
          'next
          (colorize (page-para "Actually, we can write either of those already:") BlueColor)
          (alg-code* "(define (size-2-or-4 n m)"
                     "  (or (= n 2) (= n 4)) ; @ignores @m"
                     " "
                     (format "(define (2-or-4-fish l) (~a size-2-or-4 l))" ffilter))
          'next
          (colorize (page-para "This suggests a simplification of" (alg-code ffilter)) BlueColor))))
  
  (slide/title
   "Filter"
   (page-para
    (size-func "filter" "PRED" " PRED" " (num -> bool)" ""))
   'next
   (page-para
    (alg-code* "(define (greater-than-5 n)"
               "   (> n 5))"
               " "
               "(define (big l) (filter greater-than-5 l))")))

  (slide/title
   "Local Helpers"
   (colorize (page-para "Since only" (alg-code "big") "needs to use" (alg-code "greater-than-5") ", make it local:") BlueColor)
   (alg-code* "(define (big l)"
              "  (let ([greater-than-5 (lambda (n) (> n 5))])"
              "      (filter greater-than-5 l))")
   'next
   (blank)
   (page-item "Suppose we move to Texas, where \"big\" means more than 10 pounds")
   (alg-code* "(define (texas-big l)"
              "  (let ([greater-than-10 (lambda (n) (> n 10))])"
              "      (filter greater-than-10 l))")
   'next
   (colorize (it "More cut-and-paste?!") RedColor))

  (slide/title
   "Abstraction over Local Functions"
   (page-para
    (alg-code* "(define (relatively-big l m)"
               "  (let ([greater-than-m (lambda (n) (> n m))])"
               "      (filter greater-than-m l))"
               " "
               "(define (big l) (relatively-big l 5))"
               "(define (texas-big l) (relatively-big l 10))"))
   'next
   (page-para
    (alg-code* "(big '(2 4 8 11)) = '(8 11)"
               "(texas-big '(2 4 8 11)) = '(11)"))
   'next
   (colorize (page-para* "How does that work?") BlueColor))
  
  (slide/title
   "Evaluation with Local Functions"
   'alts
   (steps (alg-code* "(define (rel-big l m)"
                     "  (let ([gt-m (Lambda (n) (> n m))])"
                     "     (filter gt-m l))"
                     " "
                     "(define (big l) "
                     "  (rel-big l 5))"
                     " ")
          (alg-code "(big '(2 4 8))")
          (alg-code "(rel-big '(2 4 8) 5)")
          (alg-code*
           "(let ([gt-m (Lambda (n) (> n 5))])"
           "  (filter gt-m '(2 4 8))")
          (alg-code*
           "(define (gt-m_98 n) (> n 5))))"
           " "
           "(filter gt-m_98 '(2 4 8))"))
   'next
   (colorize (page-para* "Every time we call" (alg-code "rel-big") "we get a brand-new"
                         (alg-code "gt-m"))
             BlueColor))
  
  (slide/title
   "Filter and Map"
   (page-item "A function like" (alg-code "filter") "is so useful that it's usually built in")
   (page-subitem "But not in the EoPL langugae, unfortunately")
   'next
   (page-item "Here's one that's even more useful (and is built in):")
   (alg-code* ";; map : (num -> num) list-of-num -> list-of-num"
              "(define (map F l)"
              "  (cond"
              "    [(null? l) '()]"
              "    [else (cons (F (car l)) (map F (cdr l)))]))"
              ""
              "(map add1 '(1 2 3)) = '(2 3 4)"))
  
  (slide/title
   "Map, More Generally"
   (colorize (page-para "Actually," (alg-code "map") "is more general") BlueColor)
   (alg-code*
    ";; map : (X -> Y) list-of-X -> list-of-Y"
    " "
    "(map even? '(1 2 3)) = '(#f #t #f)"
    "(map car '((1 2) (3 4) (5 6))) = '(1 3 5)")
   'next
   (colorize (page-para "Actually," (alg-code "map") "is" (it "more") "general!") BlueColor)
   (alg-code*
    ";; map : (X_1 ... X_n -> Y) l-o-X_1 ... l-o-X_n -> l-o-Y"
    " "
    "(map + '(1 2 3) '(4 5 6)) = '(5 7 9)"
    "(map cons '(1 2 3) '(#f #f #t)) = '((1 . #f) (2 . #f) (3 . #t))"))
  
  (slide/title/center
   "Closing Thought"
   (page-para* "Why must functions always have a name?"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  'done)
