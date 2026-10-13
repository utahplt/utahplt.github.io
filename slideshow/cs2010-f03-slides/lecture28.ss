#cs
(module lecture28 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
	   "utils/explain.ss"
           (all-except "utils/utils.ss" with-steps with-steps~))

  (define (exs . l)
    (apply hbl-append (* 3 gap-size) l))

  (define (built-in t v)
    (vc-append
     gap-size
     (page-item t)
     v
     (blank)))
  
  (slide/title
   "Java's Built-in Data Definitions"
   (built-in (ttt "int")
	      (exs (vtt "1")
		   (vtt "5999")
		   (vtt "-10")))
   (built-in (ttt "double")
             (exs (vtt "1.1")
                  (vtt "5999.33")
                  (vtt "-10.01")))
   (built-in (ttt "boolean")
             (exs (vtt "true")
                  (vtt "false")))
   (built-in (ttt "String")
             (exs (vtt "\"hello\"")
                  (vtt "\"See you later!\""))))
  
  (define snake-code
    (scale/improve-new-text
     (java "class Snake {"
           "  String name;"
           "  double weight;"
           "  String food;"
           "  Snake(String name, double weight, String food) {"
           "    this.name = name;"
           "    this.weight = weight;"
           "    this.food = food;"
           "  }"
           "}")
     0.9))
    
  (slide/title
   "Compound Data in Java"
   (page-para "Beginner Scheme:")
   (code (code:comment "A snake is")
         (code:comment " (make-snake sym num sym)")
         (define-struct snake (name weight food)))
   (page-para "Beginner Java:")
   'alts
   (list (list snake-code)
         (list (explain snake-code
                        1/10 1/10 'nw
                        (ktt "class") "starts a data definition, or a"
                        (dt "class declaration") "in Java terminology"))
         (list (explain snake-code
                        2/10 1/10 'nw
                        "Next is the name for the data definition;"
                        "by convention, the name is captalized"))
         (list (explain snake-code
                        1/4 5/100 'w
                        "Put" (ott "{") "after the name"))
         (list (explain snake-code
                        1/3 25/100 'w
                        "For each part of the compound value, write"
                        (itt "type") "then" (itt "name") "then" (ott ";")
                        ", one line for each part; this is a" (dt "field")))
         (list (explain* 1/2 snake-code
                         1/10 42/100 'sw
                         "After the parts, write the defined name again;"
                         "this starts the" (dt "constructor")))
         (list (explain snake-code
                        15/100 42/100 'sw
                        "Then a" (ott "(")))
         (list (explain snake-code
                        1/2 42/100 'sw
                        "Write each field again, but this time separate"
                        "with" (ott ",") sym:emdash "these are the"
                        (dt "constructor arguments")))
         (list (explain snake-code
                        94/100 42/100 'se
                        "Then a" (ott ")")))
         (list (explain snake-code
                        97/100 42/100 'se
                        "Then a" (ott "{")))
         (list (explain snake-code
                        1/2 65/100 'sw
                        "Each field, one more time..."
                        (ktt "this") "then" (ott ".")
                        "then" (itt "name") "then"
                        (ttt "=") "then" (itt "name")
                        "then" (ott ";")))
         (list (explain* 1/2 snake-code
                         6/100 85/100 'w
                         "Closing" (ott "}")
                         "for the constructor"))
         (list (explain* 1/2 snake-code
                         2/100 95/100 'w
                         "Closing" (ott "}")
                         "for the class declaration"))))
  
  (define snake-ex
    (java "new Snake(\"Slinky\", 12, \"rats\")"
          "new Snake(\"Slimey\", 5, \"grass\")"))
  
  (slide/title
   "Instances of Compound Data Types"
   (page-para "Beginner Scheme:")
   (code (make-snake 'Slinky 12 'rats)
         (make-snake 'Slimey 5 'grass))
   (page-para "Beginner Java:")
   'alts
   (list (list snake-ex)
         (list (explain snake-ex
                        5/100 19/20 'nw
                        (ktt "new") "starts an instance (a value) of a class"))
         (list (explain snake-ex
                        2/10 19/20 'nw
                        "Next is the class name"))
         (list (explain snake-ex
                        32/100 19/20 'nw
                        "Then" (ott "(")))
         (list (explain snake-ex
                        1/2 19/20 'n
                        "Then field values separated by" (ott ",")))
         (list (explain snake-ex
                        97/100 19/20 'ne
                        "Then" (ott ")")))))

  (slide/title
   "Armadillos"
   (java "class Dillo {"
         "  double weight;"
         "  boolean alive;"
         "  Dillo(double weight, boolean alive) {"
         "    this.weight = weight;"
         "    this.alive = alive;"
         "  }"
         "}"
         " "
         "new Dillo(2, true)"
         "new Dillo(3, false)"))

  (slide/title
   "Posns"
   (java "class Posn {"
         "  int x;"
         "  int y;"
         "  Posn(int x, int y) {"
         "   this.x = x;"
         "   this.y = y;"
         "  }"
         "}"
         " "
         "new Posn(0, 0)"
         "new Posn(1, -2)"))
  
  (slide/title
   "Ants"
   (java "class Ant {"
         "  double weight;"
         "  Posn loc;"
         "  Ant(double weight, Posn loc) {"
         "    this.weight = weight;"
         "    this.loc = loc;"
         "  }"
         "}"
         " "
         "new Ant(0.0001, new Posn(0, 0))"
         "new Ant(0.0002, new Posn(1, -2))"))

  (define animal-code
    (scale/improve-new-text
     (java "abstract class Animal {"
           "}"
           " "
           "class Snake extends Animal {"
           " ... as before ..."
           "}"
           "class Dillo extends Animal {"
           " ... as before ..."
           "}"
           "class Ant extends Animal {"
           " ... as before ..."
           "}")
     0.9))

  (define (mk-begj animal-code)
    (list
     (page-para (ht-append 
                 (* 4 gap-size)
                 (t "Beginner Java:")
                 animal-code))))

  (slide/title
   "Data with Variants"
   (page-para (ht-append
               (* 4 gap-size)
               (t "Beginner Scheme:")
               (scale/improve-new-text
                (code (code:comment "An animal is either")
                      (code:comment "  - snake")
                      (code:comment "  - dillo")
                      (code:comment "  - ant"))
                0.9)))
   (blank)
   'alts
   (list (mk-begj animal-code)
         (mk-begj (explain animal-code
                           1/10 9/120 'ne
                           (ktt "abstract") (ktt "class")
                           "for a data definition with variants"))
         (mk-begj (explain animal-code
                           1/10 15/120 'nw
                           "No fields and no constructor when"
                           "a class merely groups variants"))
         (mk-begj (explain* 1/3 animal-code
                            1/2 32/120 's
                            "Change the class for each variant"
                            "by adding" (ktt "extends")
                            "then the grouping class name, all before" (ott "{")))
         (mk-begj (explain animal-code
                           1/3 48/120 'n
                           "Nothing else changes"))))
                  

  (define (mk-grade g)
    (htl-append
     (* 3 gap-size)
     (scale/improve-new-text
      (code
       (code:comment "A grade is either")
       (code:comment " - false")
       (code:comment " - num"))
      0.8)
     (g sym:implies)
     (g
      (scale/improve-new-text
       (code
        (code:comment "A grade is either")
        (code:comment " - no-grade")
        (code:comment " - num-grade")
        code:blank
        (code:comment "A no-grade is")
        (code:comment " (make-no-grade)")
        (define-struct no-grade ())
        code:blank
        (code:comment "A num-grade is")
        (code:comment " (make-num-grade num)")
        (define-struct num-grade (n)))
       0.8))))
     

  (slide/title
   "Variants in Java"
   (page-item "A data definition with variants must refer only to other"
              "data definitions (which are not built in)")
   'next
   'alts~
   (list (list (mk-grade ghost))
         (list (mk-grade values)))
   'next
   (blank)
   (page-item "A data definition can be a variant in at most one other data definition"))

  
  )
