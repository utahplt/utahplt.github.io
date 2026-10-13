#cs
(module lecture15 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
	   "utils/utils.ss"
           "utils/code.ss"
           "utils/eval-step.ss"
           "utils/lec14.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss"))

  (slide/title
   "Symbols"
   (page-para "Our favorite" (code list-of-sym) "program:")
   (scale/improve-new-text
    (code 
     (code:contract eat-apples : list-of-sym -> list-of-sym)
     (define (eat-apples l)
       (cond
	[(empty? l) empty]
	[(cons? l)
	 (local [(define ate-rest (eat-apples (rest l)))]
           (cond
             [(symbol=? (first l) 'apple) ate-rest]
             [else (cons (first l) ate-rest)]))])))
    0.9 0.9)
   (blank)
   (page-item "How about" (code eat-bananas) "?")
   (page-item "How about" (code eat-non-apples) "?")
   'next
   (colorize
    (page-para/r "We know where this leads...")
    BlueColor))
  
  (define filter-syms-defn
    (code
     (code:contract filter-syms :  (sym -> bool) list-of-sym
                    -> list-of-sym)
     (define (filter-syms PRED l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r 
                    (filter-syms PRED (rest l)))]
            (cond
              [(PRED (first l)) 
               (cons (first l) r)]
              [else r]))]))))
  
  (slide/title
   "Filtering Symbols"
   (page-para filter-syms-defn)
   'next
   (blank)
   (colorize
    (page-para* "This looks" (it "really") "familiar")
    BlueColor))
  
  (slide/title
   "Last Time: Filtering Numbers"
   (page-para filter-nums-defn)
   'next
   (blank)
   (colorize
    (page-para* "How do we avoid cut and paste?")
    BlueColor))
  
  
  (define filter-defn/no-contract
    (code
     (define (filter PRED l)
       (cond
         [(empty? l) empty]
         [(cons? l)
          (local [(define r 
                    (filter PRED (rest l)))]
            (cond
              [(PRED (first l)) 
               (cons (first l) r)]
              [else r]))]))))

  (define filter-defn/unknown-contract
    (vl-append
     line-sep
     (code (code:contract filter :  ...))
     filter-defn/no-contract))

  (slide/title
   "Filtering Lists"
   (page-para "We know this function will work for both number and symbol lists:")
   filter-defn/unknown-contract
   (page-para "But what is its contract?"))
  
  (slide/title
   "The Contract of Filter"
   (page-para "How about this?")
   (scale/improve-new-text
    (code (num-OR-sym -> bool) list-of-num-OR-list-of-sym
          -> list-of-num-OR-list-of-sym)
    0.9 0.9)
   'alts
   (list (list (blank)
               (code (code:comment "A num-OR-sym is either")
                     (code:comment " - num")
                     (code:comment " - sym")
                     code:blank
                     (code:comment "A list-of-num-OR-list-of-sym is either")
                     (code:comment " - list-of-num")
                     (code:comment " - list-of-sym")))
         (list (blank)
               (page-para "This contract is too weak to define" (code eat-apples))
               (code (code:contract eat-apples : list-of-sym -> list-of-sym)
                     (define (eat-apples l)
                       (filter not-apple? l))
                     code:blank
                     (code:contract not-apple? : sym -> bool)
                     (define (not-apple? s)
                       (not (symbol=? s 'apple))))
               'alts
               (list (list
                      (colorize
                       (page-para (code eat-apples) "must return a" (code list-of-sym)
                                  ", but by its contract," (code filter)
                                  "might return a" (code list-of-num))
                       BlueColor))
                     (list
                      (colorize
                       (page-para (code not-apple?) "only works on symbols, but"
                                  "by its contract" (code filter) "might"
                                  "give it a" (code num))
                       BlueColor))))))
                                                                   
  (slide/title
   "The Contract of Filter"
   (page-para "The reason" (code filter) "works is that if we give it a"
              (code list-of-sym) ", then it returns a" (code list-of-sym))
   (page-para "Also, if we give" (code filter) "a" (code list-of-sym)
              ", then it calls" (code PRED) "with symbols only")
   'next
   (blank)
   (page-para "A better contract:")
   (code filter : 
         ((num -> bool) list-of-num 
          -> list-of-num)
         OR
         ((sym -> bool) list-of-sym
          -> list-of-sym))
   'next
   (page-para "But what about a list of" (hbl-append (code image) (t "s"))
              ","  (hbl-append (code posn) (t "s"))
              ", or" (hbl-append (code snake) (t "s")) "?"))
  
  (slide/title
   "The True Contract of Filter"
   (page-para "The real contract is")
   (code filter : ((X -> bool) list-of-X -> list-of-X))
   (page-para "where" (code X) "stands for any type")
   (blank)
   (page-item "The caller of" (code filter) "gets to pick a type for" (code X))
   (page-item "All" (hbl-append (code X) (t "s")) "in the contract must"
              "be replaced with the same type")
   'next
   (blank)
   (page-para "Data definitions need type variables, too:")
   (code (code:comment "A list-of-X is either")
         (code:comment " - empty")
         (code:comment " - (cons X empty)")))

  (slide/title
   "Using Filter"
   (page-para "The" (code filter) "function is so useful that it's built in")
   (blank)
   (page-para "New solution for HW 4 that works in" (bt "Intermediate") ":")
   (code (define (eat-apples l)
           (local [(define (not-apple? s)
                     (not (symbol=? s 'apple)))]
             (filter not-apple? l)))))
  
  (define map-defn/no-contract
    (code
     (define (map CONV l)
       (cond
         [(empty? l) empty]
         [else (cons (CONV (first l))
                     (map CONV (rest l)))]))))

  (slide/title
   "Looking for Other Built-In Functions"
   (page-para "Recall" (code inflate-by-4%) ":")
   (code (code:contract inflate-by-4% : list-of-num -> list-of-num)
         (define (inflate-by-4% l)
           (cond
             [(empty? l) empty]
             [else (cons (* (first l) 1.04)
                         (inflate-by-4% (rest l)))])))
   (page-para "Is there a built-in function to help?")
   'next
   (colorize
    (page-para* "Yes:" (code map))
    BlueColor))
  
  (slide/title
   "Using Map"
   (vl-append
    line-sep
    map-defn/no-contract
    (code code:blank
          code:blank
          (define (inflate-by-4% l)
            (local [(define (inflate-one n)
                      (* n 1.04))]
              (map inflate-one l)))
          code:blank
          (code:contract negate-colors : list-of-col -> list-of-col)
          (define (negate-colors l)
            (map negate-color l)))))
  
  (slide/title
   "The Contract for Map"
   map-defn/no-contract
   (page-item "The" (code l) "argument must be a list of" (code X))
   (page-item "The" (code CONV) "argument must accept each" (code X))
   (page-item "If" (code CONV) "returns a new" (code X) "each time, then the contract for" (code map) "is")
   (code map : (X -> X) list-of-X -> list-of-X))
  
  (slide/title
   "Posns and Distances"
   (page-para "Another function from HW 4:")
   (code (code:contract distances : list-of-posn -> list-of-num)
         (define (distances l)
           (cond
             [(empty? l) empty]
             [(cons? l) (cons (distance-to-0 (first l))
                              (distances (rest l)))])))
   'next
   (blank)
   (page-para "The" (code distances) "function looks just like" (code map)
              ", except that" (code distances-to-0) "is")
   (code posn -> num)
   (page-para "not")
   (code posn -> posn))
              
  (slide/title
   "The True Contract of Map"
   (page-para "Despite the contract mismatch, this works!")
   (code (define (distances l)
           (map distance-to-0 l)))
   'next
   (blank)
   (page-para "The true contract of" (code map) "is")
   (code map : (X -> Y) list-of-X -> list-of-Y)
   (blank)
   (page-para "The caller gets to pick both" (code X)
              "and" (code Y) "independently"))

  (slide/title
   "More Uses of Map"
   (page-para
    (code (code:contract modernize : list-of-pipe -> list-of-pipe)
          (define (modernize l)
            (code:comment "replaces 4 lines:")
            (map modern-pipe l))
          code:blank
          (code:contract modern-pipe : pipe -> pipe)
          ...))
   (blank)
   (page-para
    (code (code:contract rob-train : list-of-car -> list-of-car)
          (define (rob-train l)
            (code:comment "replaces 4 lines:")
            (map rob-car l))
          code:blank
          (code:contract rob-car : car -> car)
          ...)))

  (slide/title
   "Folding a List"
   (page-para "How about" (code sum) "?")
   (code sum : list-of-num -> num)
   (page-para "Doesn't return a list, so neither" (code filter) "nor"
              (code map) "help")
   'next
   (blank)
   (page-para "But recall" (code combine-nums) "...")
   (code
    (code:contract combine-nums : list-of-num num 
                   (num num -> num) -> num)
    (define (combine-nums l base-n COMB)
      (cond
        [(empty? l) base-n]
        [(cons? l) 
         (COMB 
          (first l)  
          (combine-nums (rest l) base-n COMB))]))))

  (define fold-defn
    (code
     (define (foldr COMB base l)
       (cond
         [(empty? l) base]
         [(cons? l) 
          (COMB (first l)
                (foldr COMB base (rest l)))]))))
    
  (slide/title
   "The Foldr Function"
   (vl-append
    line-sep
    (code (code:contract foldr : (X Y -> Y) Y list-of-X -> Y))
    fold-defn)
   'next
   'alts
   (list (list (blank)
               (page-para "The" (code sum) "and" (code product) "functions become trivial:")
               (code (define (sum l) (foldr + 0 l))
                     (define (product l) (foldr * 1 l))))
         (list (blank)
               (page-para "Useful for HW 5:")
               (code
                (code:contract total-blue : list-of-col -> num)
                (define (total-blue l)
                  (local [(define (add-blue c n)
                            (+ (color-blue c) n))]
                    (foldr add-blue 0 l)))))
         (list (blank)
               (page-para "In fact,")
               (code (define (map f l)
                       (local [(define (comb i r)
                                 (cons (f i) r))]
                         (foldr comb empty l)))))
         (list (blank)
               (page-para "Yes," (code filter) "too:")
               (code
                (define (filter f l)
                  (local [(define (check i r)
                            (cond
                              [(f i) (cons i r)]
                              [else r]))]
                    (foldr check empty l)))))))
  
  (slide/title
   "The Source of Foldr"
   'alts
   (list (list (page-para "How can" (code foldr) "be so powerful?"))
         (list
          (page-para "Template:")
          (page-para
           (code (define (func-for-loX l)
                   (cond
                     [(empty? l) ...]
                     [(cons? l) ... (first l)
                      ... (func-for-loX (rest l)) ...]))))          
          (blank)
          (page-para "Fold:")
          fold-defn)))
  
  (slide/title
   "Other Built-In List Functions"
   (page-para "More specializations of" (code foldr) ":")
   (code ormap : (X -> bool) list-of-X -> bool)
   (code andmap : (X -> bool) list-of-X -> bool)
   (page-para "Examples:")
   (code (code:contract got-milk? : list-of-sym -> bool)
         (define (got-milk? l)
           (local [(define (is-milk? s)
                     (symbol=? s 'milk))]
             (ormap is-milk? s)))
         code:blank
         (code:contract all-passed? : list-of-grade -> bool)
         (define (all-passed? l)
           (andmap passing-grade? l))))

  (slide/title
   "What about Non-Lists?"
   (page-para "Since it's based on the template, the concept of fold is general")
   (scale/improve-new-text
    (code 
     (code:contract fold-ftn : (sym num sym Z Z -> Z) Z ftn -> Z)
     (define (fold-ftn COMB base ftn)
       (cond
         [(empty? ftn) base]
         [(child? ftn)
          (COMB (child-name ftn) (child-date ftn) (child-eyes ftn)
                (fold-ftn COMB BASE (child-father ftn))
                (fold-ftn COMB BASE (child-mother ftn)))]))
     code:blank
     (define (count-persons ftn)
       (local [(define (add name date color c-f c-m)
                 (+ 1 c-f c-m))]
         (fold-ftn add 0 ftn)))
     code:blank
     (define (in-family? who ftn)
       (local [(define (here? name date color in-f? in-m?)
                 (or (symbol=? name who) in-f? in-m?))]
         (fold-ftn here? false ftn))))
    0.75))
   
  
  )
