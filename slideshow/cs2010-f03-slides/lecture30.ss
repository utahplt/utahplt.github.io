#cs
(module lecture30 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/run.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (define (mk-scheme-animal s)
    (scale/improve-new-text
     (code
      (code:comment "An animal is either")
      (code:comment " - snake")
      (code:comment " - dillo")
      (code:comment " - ant")
      code:blank
      (code:contract animal-is-lighter? : animal num -> bool)
      (define (animal-is-lighter? a n)
        (cond
          [(snake? a) (snake-is-lighter? s n)]
          [(dillo? a) (dillo-is-ligheter? s n)]
          [(ant? a) (ant-is-lighter? s n)]))
      code:blank
      (code:contract snake-is-lighter? : snake num -> bool)
      (define (snake-is-lighter? s n) ...)
      code:blank
      (code:contract dillo-is-lighter? : dillo num -> bool)
      (define (dillo-is-lighter? d n) ...)
      code:blank
      (code:contract ant-is-lighter? : ant num -> bool)
      (define (ant-is-lighter? a n) ...))
     s))
  
  (slide/title
   "Functions with Variants"
   (mk-scheme-animal 0.75))
  
  (define (mk-java-animal s)
    (scale/improve-new-text
     (java
      "abstract class Animal {"
      "  abstract boolean isLighter(double n);"
      "}"
      " "
      "class Snake extends Animal {"
      "  ..."
      "  boolean isLighter(double n) { ... }"
      "}"
      " "
      "class Dillo extends Animal {"
      "  ..."
      "  boolean isLighter(double n) { ... }"
      "}"
      " "
      "class Ant extends Animal {"
      "  ..."
      "  boolean isLighter(double n) { ... }"
      "}")
     s))

  (slide/title
   "Methods with Variants"
   (mk-java-animal 0.75))
  
  (define sm-scheme (mk-scheme-animal 0.55))
  (define sm-java (mk-java-animal 0.55))
  
  (define (trans extend thought)
    (slide/title
     "Translating Functions to Methods"
     (let ([p (rt-superimpose
               thought
               (lt-superimpose
                sm-scheme
                (rb-superimpose
                 sm-java
                 titleless-page)))])
       (extend p sm-scheme sm-java))))
  
  (define (hilite p sx sy sx2 sy2 s)
    (let* ([f0 (colorize (linewidth 
                          2 
                          (frame (blank (* (pict-width s) (- sx2 sx))
                                        (* (pict-height s) (- sy2 sy)))))
                         RedColor)]
           [f (inset
               f0
               (* (pict-width s) sx)
               (* (pict-height s) sy)
               0
               0)])
      (values (place-over p s find-lt f) f0)))
  
  (define (thought . l)
    (apply para (* 1/2 client-w) l))
  
  (trans (lambda (p s j) p) (blank))
  (trans (lambda (p s j)
           (let*-values ([(p lf) (hilite p 0 0/20 2/3 4/20 s)]
                         [(p r1) (hilite p 0 0/18 11/20 1/18 j)]
                         [(p r2) (hilite p 0 4/18 2/3 5/18 j)]
                         [(p r3) (hilite p 0 9/18 2/3 10/18 j)]
                         [(p r4) (hilite p 0 14/18 15/24 15/18 j)]
                         [(as) gap-size])
             (add-arrow-line 
              as
              (add-arrow-line 
               as
               (add-arrow-line 
                as
                (add-arrow-line as p lf find-rb r1 find-lt 2 RedColor)
                lf find-rb r2 find-lt 2 RedColor)
               lf find-rb r3 find-lt 2 RedColor)
              lf find-rb r4 find-lt 2 RedColor)))
         (thought "Data definition turns into class declarations"))
  (trans (lambda (p s j)
           (let*-values ([(p l1) (hilite p 0 13/20 55/60 14/20 s)]
                         [(p l2) (hilite p 0 16/20 55/60 17/20 s)]
                         [(p l3) (hilite p 0 19/20 55/60 20/20 s)]
                         [(p r1) (hilite p 5/100 6/18 95/100 7/18 j)]
                         [(p r2) (hilite p 5/100 11/18 95/100 12/18 j)]
                         [(p r3) (hilite p 5/100 16/18 95/100 17/18 j)]
                         [(as) gap-size])
             (add-arrow-line
              as
              (add-arrow-line 
               as
               (add-arrow-line as p l1 find-rc r1 find-lc 2 RedColor)
               l2 find-rc r2 find-lc 2 RedColor)
              l3 find-rc r3 find-lc 2 RedColor)))
         (thought "Variant functions turn into variant methods"
                  sym:emdash "all with the same contract after the implicit argument"))
  (trans (lambda (p s j)
           (let*-values ([(p lf) (hilite p 0 5/20 1 11/20 s)]
                         [(p rf) (hilite p 5/100 1/18 1 2/18 j)]
                         [(as) gap-size])
               (add-arrow-line as p lf find-rc rf find-lc 2 RedColor)))
         (thought "Function with variant-based"
                  (code cond) "turns into just an"
                  (java "abstract") "method declaration"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (list-code list-string)
    (java/copy
     0.8
     "abstract class ListOfThing {"
     "  abstract int length();"
     "}"
     " "
     "class EmptyListOfThing extends ListOfThing {"
     "   EmptyListOfThing() { }"
     "   int length() { return 0; } "
     "}"
     " "
     "class ConsListOfThing extends ListOfThing {"
     "   Thing first;"
     "   ListOfThing rest;"
     "   ConsListOfThing(Thing first, ListOfThing rest) {"
     "     this.first = first;"
     "     this.rest = rest;"
     "   }"
     "   int length() { return 1 + this.rest.length(); } "
     "}"))
  
  (slide/title
   "Lists of Things"
   list-code
   (mk-copy list-string))

  (define-values (tree-code tree-string)
    (java/copy
     0.7
     "abstract class TreeOfThing {"
     "  abstract int count();"
     "}"
     " "
     "class EmptyTreeOfThing extends TreeOfThing {"
     "   EmptyTreeOfThing() { }"
     "   int count() { return 0; } "
     "}"
     " "
     "class ConsTreeOfThing extends TreeOfThing {"
     "   Thing v;"
     "   TreeOfThing left;"
     "   TreeOfThing right;"
     "   ConsTreeOfThing(Thing v, TreeOfThing left, TreeOfThing right) {"
     "     this.v = v;"
     "     this.left = left;"
     "     this.right = right;"
     "   }"
     "   int count() { return 1 + this.left.count()"
     "                          + this.right.count(); } "
     "}"))
  
  (slide/title
   "Trees of Things"
   (cb-superimpose
    tree-code
    (mk-copy tree-string)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Implementing Methods Directly"
   (page-para "Some Scheme methods on" (code animal)
              "can be implemented with other" (code animal)
              "functions:")
   (code (code:contract animal-light? : animal -> bool)
         (code:comment "Determines whether a is less than 10 lbs")
         (define (animal-light? a)
           (animal-lighter? a 10)))
   'next
   (page-para "In Java, this corresponds to a"
              (hbl-append (t "non-") (java "abstract"))
              "method in an" (java "abstract") "class:")
   (java
    "abstract class Animal {"
    "   ..."
    "   boolean isLight() {"
    "     return this.isLighter(10);"
    "   }"
    "}"))
  
  (slide/title
   "Conditionals"
   (page-para "Some uses of" (code cond) "where not based on the data definition:")
   (code
    (define (posn-big-part p)
      (cond
        [(> (posn-x p) (posn-y p)) (posn-x p)]
        [else (posn-y p)])))
   'next
   (page-para "For these, we use" (java "if") "in Java:")
   (java 
    "class Posn { ..."
    "  double bigPart() {"
    "    if (this.x > this.y)"
    "      return this.x;"
    "    else"
    "      return this.y;"
    "  }"
    "}"))

  (slide/title
   "Conditionals"
   (page-para "In general:")
   (ht-append
    (* gap-size 2)
    (code
     (cond
       [_question1 _answer1]
       [_question2 _answer2]
       ...
       [else _answerN]))
    sym:implies
    (java
     "if _question1"
     "  return _answer1;"
     "else if _question2"
     "  return _answer2;"
     "..."
     "else"
     "  return _answerN;")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title/center
   "Primitive Operations on Numbers"
   (java
    "1 + 2  \"should be\" 3"
    "1 - 2  \"should be\" -1"
    "1 * 2  \"should be\" 2"
    "1 / 2  \"should be\" 0"
    "1.0 / 2.0  \"should be\" 0.5"
    " "
    "1 < 2  \"should be\" true"
    "1 > 2  \"should be\" false"
    "1 <= 2  \"should be\" true"
    "1 >= 2  \"should be\" false"
    "1 == 2  \"should be\" false"
    "1 == 1  \"should be\" true"))

  (slide/title/center
   "Primitive Operations on Booleans"
   (java
    "!true \"should be\" false"
    "!false \"should be\" true"
    " "
    "true && true \"should be\" true"
    "true && false \"should be\" false"
    "true || false \"should be\" true"
    "false || false \"should be\" false"))
    
  (slide/title/center
   "Primitive Operations on Strings"
   (java
    "\"hello\".equals(\"bye\") \"should be\" false"
    "\"hello\".equals(\"hello\") \"should be\" true"
    " "
    "\"good\".concat(\" bye\") \"should be\" \"good bye\""
    " "
    "\"good bye\".startsWith(\"good\") \"should be\" true"
    "\"good bye\".startsWith(\"bye\") \"should be\" false"
    " "
    "\"good bye\".endsWith(\"good\") \"should be\" false"
    "\"good bye\".endsWith(\"bye\") \"should be\" true")
   (blank)
   (colorize
    (para* (* 4/5 client-w)
           "These operations are really method calls, and"
           "that's why" (java "String") "is capitalized")
    BlueColor))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Testing Java Code"
   (page-para "Select" (bt "New Test Suite") "from the" (bt "File") "menu")
   (bitmap "test-tool.png"))
  
  (slide/title
   "Testing Tool Warnings"
   (page-item "The test tool is tied to the program via the file name")
   (page-subitem "If you rename your file, you must fix the test window")
   (page-subitem "You must save changes to your program before testing")
   (blank)
   (page-item "To open a saved test suite, use" (bt "Open Test Suite..."))
   (blank)
   (page-item "The test tool incorrectly reports failures when the result is not"
              "a number or boolean")))
