#cs
(module lecture29 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/explain.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
	   (lib "mred.ss" "mred") (lib "class.ss"))

  (define (mk-snake-defn/java . extra)
    (scale/improve-new-text
     (apply
      java 
      "class Snake {"
      "  String name;"
      "  double weight;"
      "  String food;"
      "  Snake(String name, double weight, String food) {"
      "    this.name = name;"
      "    this.weight = weight;"
      "    this.food = food;"
      "  }"
      (append (map (lambda (s) (string-append "  " s)) extra)
              (list "}")))
     0.9))
  
  (define snake-defn/scheme
    (code (code:comment "A snake is")
          (code:comment " (make-snake sym num sym)")
          (define-struct snake (name weight food))))
  
  (slide/title
   "From Scheme to Java"
   (page-para "So far, we've translated data definitions:")
   snake-defn/scheme
   (page-para sym:implies)
   (mk-snake-defn/java))
  
  (define snake-code/scheme
    (code
     (code:contract snake-lighter? : snake num -> bool)
     (code:comment " Determines whether s is < n lbs")
     (define (snake-lighter? s n)
       (< (snake-weight s) n))))

  (define lighter-call/scheme
    (scale/improve-new-text
     (code (snake-lighter? (make-snake 'Slinky 10 'rats) 10)
           "should be" false)
     0.9))

  (define lighter-call/alt/scheme
    (scale/improve-new-text
     (code (define SLINKY (make-snake 'Slinky 10 'rats))
           code:blank
           (snake-lighter? SLINKY 10)
           "should be" false)
     0.9))

  (slide/title
   "Functions in Scheme"
   (vl-append
    line-sep
    snake-defn/scheme
    (code code:blank)
    snake-code/scheme
    (code code:blank)
    lighter-call/scheme
    (scale/improve-new-text
     (code (snake-lighter? (make-snake 'Slimey 5 'grass) 10)
           "should be" false)
     0.9)))

  (define method-strs
    '("// Determines whether it's < n lbs"
      "boolean isLighter(double n) {"
      "  return this.weight < n;"
      "}"))
  
  (define lighter-call/java
    (scale/improve-new-text
      (java
       "new Snake(\"Slinky\", 10, \"rats\").isLighter(10)"
       "\"should be\" false")
      0.9))
  
  (define lighter-call/alt/java
    (scale/improve-new-text
      (java
       "Snake slinky = new Snake(\"Slinky\", 10, \"rats\");"
       " "
       "slinky.isLighter(10)"
       "\"should be\" false")
      0.9))
  
  (define snake-code/java
    (vl-append
     line-sep
     (apply
      mk-snake-defn/java
      " "
      method-strs)
     (scale/improve-new-text
      (java
       " ")
      0.9)
     lighter-call/java))
  
  (slide/title/tall
   "Functions in Java"
   'alts
   (list (list snake-code/java)
         (list (explain snake-code/java
                        3/10 112/180 'sw
                        "A function becomes a"
                        (dt "method") "that is in the"
                        (ktt "class")))))

  (define snake-method/java
    (apply java method-strs))

  (slide/title
   "Methods in Java"
   (page-para "Comparing just the function and method:")
   (page-para "  Scheme:")
   snake-code/scheme
   (page-para "  Java:")
   'alts
   (list (list snake-method/java)
         (list (explain snake-method/java
                        105/200 15/40 'sw
                        "A method in" (java "Snake") "has an implicit"
                        (java "Snake this")
                        "argument"))
         (list (explain snake-method/java
                        120/200 12/40 's
                        "All other arguments are explicit, and the"
                        "type is next to the name, as in"
                        (java "double n")))
         (list (explain snake-method/java
                        1/6 12/40 'sw
                        "The result type is"
                        (java "boolean")))
         (list (explain* 0.5
                         snake-method/java
                         1/3 12/40 's
                         "Since the method takes a"
                         (java "Snake") "and" (java "double") "and produces a"
                         (java "boolean") ", the contract is"
                         (java "Snake double -> boolean")
                         "and we don't write it as a comment"))
         (list (explain* 0.35 snake-method/java
                        1/3 2/40 'sw
                        "Purpose comment is as in Scheme, but"
                        "comments start with" (java "//")))
         (list (explain snake-method/java
                        1/2 22/40 'sw
                        "Instead of" (code (snake-weight s))
                        "use" (java "this.weight")))
         (list (explain* 0.5 snake-method/java
                        1/6 22/40 'sw
                        "Explicitly designate the result"
                        "with" (java "return")))))

  (slide/title
   "Methods in Java, Step-by-Step"
   (page-para "Inside the" (java "class") "declaration...")
   'alts
   (list (list snake-method/java)
         (list (explain* 0.35 snake-method/java
                         1/10 8/40 'nw
                         "First the purpose, starting with" (java "//")))
         (list (explain snake-method/java
                        1/5 18/40 'nw
                        "Then the result type"))
         (list (explain* 0.5 snake-method/java
                        1/3 18/40 'n
                        "Then the method name (not capitalized,"
                        "by convention)"))
         (list (explain snake-method/java
                        1/2 18/40 'ne
                        "Start arguments with" (java "(")))
         (list (explain snake-method/java
                        11/20 18/40 'ne
                        "Arguments except for" (java "this") sym:emdash
                        "use a type for each argument, and separate"
                        "multiple arguments with" (java ",")))
         (list (explain snake-method/java
                        152/200 18/40 'ne
                        "End arguments with" (java ")")))
         (list (explain snake-method/java
                        165/200 18/40 'ne
                        "Then a" (java "{")))
         (list (explain snake-method/java
                        1/6 28/40 'nw
                        "Body using Java notation, put"
                        (java "return") "before a result"))
         (list (explain snake-method/java
                        142/200 28/40 'ne
                        "Put" (java ";") "after a result"))
         (list (explain snake-method/java
                        1/25 38/40 'nw
                        "End with" (java "}")))))
  
  (slide/title
   "Method Calls in Java"
   (page-para "Original tests:")
   (page-para "  Scheme:")
   lighter-call/scheme
   (blank)
   (page-para "  Java:")
   lighter-call/java)

  (slide/title
   "Method Calls in Java"
   (page-para "Equivalent, using constant definitions:")
   (page-para "  Scheme:")
   lighter-call/alt/scheme
   (blank)
   (page-para "  Java:")
   'alts 
   (list 
    (list lighter-call/alt/java)
    (list (explain lighter-call/alt/java
                   1/10 08/40 'nw
                   "Constant definition starts with the constant's type"))
    (list (explain lighter-call/alt/java
                   1/6 08/40 'nw
                   "Then the name"))
    (list (explain lighter-call/alt/java
                   12/40 08/40 'nw
                   "Then" (java "=")))
    (list (explain lighter-call/alt/java
                   1/3 08/40 'n
                   "Then an expression"))
    (list (explain lighter-call/alt/java
                   98/100 08/40 'ne
                   "End with" (java ";")))
    (list (explain* 0.7 lighter-call/alt/java
                    1/10 28/40 'nw
                    "Method call starts with an expression for the"
                    "implicit" (java "this") "argument"))
    (list (explain lighter-call/alt/java
                   14/100 28/40 'nw
                   "Then" (java ".")))
    (list (explain lighter-call/alt/java
                   24/100 28/40 'nw
                   "Then the method name"))
    (list (explain lighter-call/alt/java
                   35/100 28/40 'n
                   "Then" (java "(")))
    (list (explain* 0.7 lighter-call/alt/java
                    37/100 28/40 'n
                    "Then expressions for the explicit arguments separated by" (java ",")))
    (list (explain lighter-call/alt/java
                   41/100 28/40 'n
                   "Then" (java ")")))
    
    ))
  
  (slide/title
   "Templates"
   (page-para "In Scheme:")
   (vl-append
    line-sep
    snake-defn/scheme
    (code code:blank
          (code:comment "func-for-snake : snake -> ...")
          (define (func-for-snake s)
            ... (snake-name s)
            ... (snake-weight s)
            ... (snake-food s) ...))))
  
  (slide/title
   "Templates"
   (page-para "Same idea works for Java:")
   (mk-snake-defn/java
    " "
    "... methodForSnake(...) {"
    "  ... this.name"
    "  ... this.weight"
    "  ... this.food ..."
    "}"))

  (slide/title
   "More Examples"
   (problem "Implement a" (java "feed") "method for"
            (java "Snake") "which takes an amount of food in pounds"
            "and produces a fatter snake")
   (problem "Implement a" (java "feed") "method for"
            (java "Dillo") "and" (java "Ant"))
   (problem "Implement a" (java "feed") "method for"
            (java "Animal")))
  
  (slide/title
   "Lists in Java"
   (problem "Translate the" (code list-of-num)
            "data definition to Java and implement"
            "a" (java "length") "method")
   #;
   (problem "Translate the" (code list-of-sym)
            "data definition to Java and implement"
            "a" (java "gotMilk") "method"))
  
  )
