#cs
(module lecture32 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/run.ss"
           "utils/obj.ss"
           "utils/explain.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           (lib "step.ss" "slideshow")
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (define outline
    (make-outline
     'uml "Class Diagrams" #f
     'path "Nesting Variants to Refine Contracts" #f
     'simplify "Common Functionality in Abstract Classes" #f
     'hier "Nesting without Abstract" #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'uml)
  
  (define (islighter-proto) (sjava "boolean isLighter(double)"))
  (define (abs-islighter-proto) (sjava "boolean _isLighter(double)"))
  (define (ep-proto) (sjava "Path escapePath(Person)"))
  (define (abs-ep-proto) (sjava "Path _escapePath(Person)"))
  
  (define (sjava . l)
    (scale/improve-new-text 
     (apply java l)
     0.75))
  
  (define animal-classes
    (tree-append
     (mk-class (sjava "_Animal")
               (list (blank))
               (list (abs-islighter-proto)
                     (sjava "boolean isLight()")))
     (mk-class (sjava "Snake")
               (list (sjava "String name")
                     (sjava "double weight")
                     (sjava "String food"))
               (list (islighter-proto)
                     (sjava "boolean likesFood(String)")))
     'down
     (mk-class (sjava "Dillo")
               (list (sjava "double weight")
                     (sjava "boolean alive"))
               (list (islighter-proto)
                     (sjava "Dillo runOver()")))
     (mk-class (sjava "Ant")
               (list (sjava "double weight")
                     (sjava "Posn loc"))
               (list (islighter-proto)
                     (sjava "Ant move(int, int)")))))
  
  (slide/title
   "Animal Classes"
   (rb-superimpose
    animal-classes
    (mk-class (sjava "Posn")
              (list (sjava "double x")
                    (sjava "double y"))
              (list (blank)))))

  (slide/title
   "Some Maze Classes"
   (mk-class (sjava "Room")
             (list (sjava "Door left")
                   (sjava "Door right"))
             (list (ep-proto)))
   (blank)
   (blank)
   (mk-class (sjava "Person")
             (list (sjava "String dest")
                   (sjava "double height"))
             (list (sjava "boolean isDest(String)")
                   (sjava "boolean isShorter(double)"))))

  (define door-class
    (mk-class (sjava "_Door")
              (list (blank))
              (list (abs-ep-proto))))
  
  (define escape-class
    (mk-class (sjava "Escape")
              (list (sjava "String name"))
              (list (ep-proto))))
  
  (define (door-classes short? locked?)
    (tree-append
     door-class
     escape-class
     (cond
       [locked? 'down-left]
       [short? 'down]
       [else 'nothing])
     (if short?
         (mk-class (sjava "Short")
                   (list (sjava "Room next")
                         (sjava "double height"))
                   (list (ep-proto)))
         'nothing)
     (cond
       [locked? 'down-right]
       [else 'nothing])
     (if locked?
         (mk-class (sjava "Locked")
                   (list (sjava "Room next")
                         (sjava "String keyColor"))
                   (list (ep-proto)))
         'nothing)
     (mk-class (sjava "Into")
               (list (sjava "Room next"))
               (list (ep-proto)))))
   
  (with-steps
   (init short locked)
   (slide/title
    "Door Classes"
    (door-classes (after? short) (after? locked))))

  (define isok-proto (sjava "boolean isOk()"))
  (define abs-isok-proto (sjava "boolean _isOk()"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'path)
  
  (slide/title
   "Path Classes"
   (tree-append
    (mk-class (sjava "_Path")
              (list (blank))
              (list abs-isok-proto))
    (mk-class (sjava "Fail")
              (list (blank))
              (list isok-proto))
    (mk-class (sjava "Success")
              (list (blank))
              (list isok-proto))
    (mk-class (sjava "Left")
              (list (sjava "Path rest"))
              (list isok-proto))
    (mk-class (sjava "Right")
              (list (sjava "Path rest"))
              (list isok-proto)))
   'next
   (blank)
   'alts
   (list (list (page-para "No escape:")
               (java "new Fail()"))
         (list (page-para "Door is an immediate escape:")
               (java "new Success()"))
         (list (page-para "Turn left, then right, then you're there:")
               (java "new Left(new Right(new Success()))"))
         (list (page-para "What's this?")
               (java "new Left(new Right(new Fail()))")
               'next
               (blank)
               (page-para "We'd prefer to ensure that" (java "Left") "and" (java "Right")
                          "to extend only successful paths"))))
  
  (define (dpage-item . l)
    (apply item (* 3/4 client-w) l))
  (define (dpage-subitem . l)
    (apply subitem (* 3/4 client-w) l))
  
  (define new-path-defn
    (vl-append
     line-sep
     (dpage-item "A path is either")
     (dpage-subitem "failure")
     (dpage-subitem "success")
     (dpage-item "A success is either")
     (dpage-subitem "immediate")
     (dpage-subitem "left followed by success")
     (dpage-subitem "right followed by success")))
  
  (slide/title
   "Paths Reconsidered"
   (page-para "Our current definition:")
   (vl-append
    line-sep
    (dpage-item "A path is either")
    (dpage-subitem "failure")
    (dpage-subitem "immediate success")
    (dpage-subitem "left followed by a path")
    (dpage-subitem "right followed by a path"))
   'next
   (page-para "A better definition:")
   new-path-defn)
  
  (slide/title
   "Nested Variants"
   new-path-defn
   (blank)
   (page-para "To translate this into Java, a variant of the abstract class"
              (java "Path") "must itself be an abstract class with variants"))

  (define (path-classes isoks success-isok-proto . more)
    (apply
     slide/title
     "Revised Path Classes"
     (tree-append
      (mk-class (sjava "_Path")
                (list (blank))
                (list abs-isok-proto))
      (mk-class (sjava "Fail")
                (list (blank))
                (list isok-proto))
      'gap
      (tree-append/keep-root
       #f #t
       (mk-class (sjava "_Success")
                 (list (blank))
                 (list success-isok-proto))
       (mk-class (sjava "Immediate")
                 (list (blank))
                 isoks)
       (mk-class (sjava "Left")
                 (list (sjava "Success rest"))
                 isoks)
       (mk-class (sjava "Right")
                 (list (sjava "Success rest"))
                 isoks)))
     more))
  
  (path-classes (list isok-proto)
                abs-isok-proto
                'next
                (blank)
                (page-para "For the" (java "Success") "classes, the"
                           (java "isOk") "method always returns" (java "true") ", so"
                           "we can simplify..."))
  (path-classes (list (blank)) isok-proto)
    
  (define-values (path-code path-str)
    (java/copy
     0.55
     "abstract class Path {"
     "  abstract boolean isOk();"
     "}"
     " "
     "class Fail extends Path {"
     "  Fail() { }"
     "  boolean isOk() { return false; }"
     "}"
     " "
     " "
     "abstract class Success extends Path {"
     "  boolean isOk() { return true; }"
     "}"
     " "
     "class Immediate extends Success {"
     "  Immediate() { }"
     "}"
     " "
     "class Right extends Success {"
     "  Success rest;"
     "  Right(Success rest) { this.rest = rest; }"
     "}"
     " "
     "class Left extends Success {"
     "  Success rest;"
     "  Left(Success rest) { this.rest = rest; }"
     "}"))
  
  (slide/title
   "Revised Path Class Code"
   (cb-superimpose
    path-code
    (mk-copy path-str)))
    
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'simplify)
  
  #;
  (slide/title
   "Simplifying in Abstracts"
   (page-para "So far, we have put methods in an abstract class"
              "when the method is contract or uses only other methods"
              "in the class")
   (page-para "Sometimes, we can move more functionality into an"
              "abstract class"))
  
  (slide/title/tall
   "Common Animal Behavior"
   (page-para "All animals have a" (java "weight") "field:")
   animal-classes)

  (define new-animal-classes
    (tree-append
     (mk-class (sjava "_Animal")
               (list (sjava "double weight"))
               (list (islighter-proto)
                     (sjava "boolean isLight()")))
     (mk-class (sjava "Snake")
               (list (sjava "String name")
                     (sjava "String food"))
               (list (sjava "boolean likesFood(String)")))
     'down
     (mk-class (sjava "Dillo")
               (list (sjava "boolean alive"))
               (list (sjava "Dillo runOver()")))
     (mk-class (sjava "Ant")
               (list (sjava "Posn loc"))
               (list (sjava "Ant move(int, int)")))))

  (slide/title
   "Common Animal Behavior"
   (page-para "We can move the common field into the" (java "Animal")
              "class:")
   new-animal-classes)
  
  (define-values (animal-code animal-str)
    (java/copy
     1.0
     "abstract class Animal {"
     "  double weight;"
     "  Animal(double weight) {"
     "    this.weight = weight;"
     "  }"
     "  boolean isLighter(int n) {"
     "    return this.weight < n;"
     "  }"
     "  boolean isLight() {"
     "    return this.isLighter(10);"
     "  }"
     "}"))
    
  (slide/title
   "Fields in Abstract Classes"
   (page-para "An" (java "abstract") "class with a field needs a constructor:")
   animal-code
   (mk-copy animal-str))

  (define-values (snake-code snake-str)
    (java/copy
     0.8
     "class Snake extends Animal {"
     "  String name;"
     "  String food;"
     "  Snake(String name, double weight, String food) {"
     "    super(weight);"
     "    this.name = name;"
     "    this.food = food;"
     "  }"
     "  boolean likesFood(String s) {"
     "    return this.food.equals(s);"
     "  }"
     "}"))

  (with-steps
   (orig super profj)
   (slide/title
    "Classes that extend a Class with Fields"
    (page-para "Extensions of" (java "Animal") "must now supply the"
               (java "super") "class with its field:")
    (cond
      [(after? profj)
       (explain snake-code
                1/6 48/120 'nw
                "A" (java "super") "call must appear before the others statements")]
      [(after? super)
       (explain snake-code
                1/6 48/120 'nw
                "The" (java "super") "keyword in a constructor"
                "calls the extended class's constructor")]
      [else snake-code])
    (mk-copy snake-str)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'hier)
  
  (slide/title
   "More Common Features"
   (door-classes #t #t)
   'next
   (blank)
   (page-para "Most new kinds of door will have a"
              (java "next") "field, like" (java "Into")))

  (define into-class
    (mk-class (sjava "_Into")
              (list (sjava "Room next"))
              (list (abs-ep-proto))))
  (define short-class
    (mk-class (sjava "Short")
              (list (sjava "double height"))
              (list (ep-proto))))
  (define locked-class
    (mk-class (sjava "Locked")
              (list (sjava "String keyColor"))
              (list (ep-proto))))
  
  (slide/title/tall
   "Doors"
   (tree-append
    door-class
    escape-class
    (tree-append/keep-root
     #f #t
     into-class
     (mk-class (sjava "Plain")
               (list (blank))
               (list (ep-proto)))
     'down
     short-class
     locked-class))
   'next
   (scale/improve-new-text
    (page-para "The" (java "escapePath") "method isn't always the same,"
	       "but the" (java "this.next.escapePath(p)") "part is always the same...")
    0.8))

  (define-values (into-code into-str)
    (java/copy
     0.8
     "abstract class Into extends Door {"
     "  Room next;"
     "  Into(Room next) {"
     "    this.next = next;"
     "  }"
     "  Path escapePath(Person p) {"
     "    return this.next.escapePath(p);"
     "  }"
     "}"))

  (define-values (short-code short-str)
    (java/copy
     0.8
     "class Short extends Into {"
     "  double height;"
     "  Short(Room next, double height) {"
     "    super(next);"
     "    this.height = height;"
     "  }"
     "  Path escapePath(Person p) {"
     "   if (p.height <= this.height)"
     "     return super.escapePath(p);"
     "   else"
     "     return new Fail();"
     "  }"
     "}"))

  (slide/title
   "Method Parts in Abstract Classes"
   into-code
   (mk-copy into-str)
   (blank)
   (page-para "Note that" (java "escapePath") "is not" (java "abstract")))

  (with-steps
   (orig override super clear)
   (slide/title
    "Chaining to a Super Method"
    (if (only? super)
        (explain* 1/2 
                  short-code
                  1/2 82/130 's
                  "Using the" (java "super") "keyword"
                  "in" (java "super.escapePath")
                  "means to call the extended class's method")
        short-code)
    (mk-copy short-str)
    (blank)
    ((vafter override)
     (page-para "The" (java "escapePath") "in" (java "Short")
                (dt "overrides") "the method in" (java "Into")))))
  
  (with-steps
   (orig useless simpler nothing)
   (slide/title
    "Plain Door"
    (lt-superimpose
     ((vbefore simpler)
      (java
       "class Plain extends Into {"
       "  Plain(Room next) {"
       "    super(next);"
       "  }"
       "  Path escapePath(Person p) {"
       "   return super.escapePath(p);"
       "  }"
       "}"))
     ((vafter simpler)
      (java
       "class Plain extends Into {"
       "  Plain(Room next) {"
       "    super(next);"
       "  }"
       "}")))
    ((vafter useless)
     (page-para "The overriding" (java "escapePath")
                "merely chains to" (java "super") ", so it isn't needed"))
    ((vafter nothing)
     (page-para "In fact, we can do away with the"
                (java "Plain") "class completely, and just make"
                (java "Into") (hbl-append (t "non-") (java "abstract"))))))

  (slide/title
   "Doors Revised"
   (tree-append
    door-class
    escape-class
    (tree-append/keep-root
     #f #t
     (mk-class (sjava "Into")
               (list (sjava "Room next"))
               (list (ep-proto)))
     short-class
     locked-class)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (slide/title
   "Summary"
   (page-item "An" (java "abstract") (java "class") "can extend an" (java "abstract") (java "class"))
   (page-item "An" (java "abstract") (java "class") "can declare fields")
   (page-item "A" (java "class") "can extend a" (java "class"))
   (page-item "Use" (java "super(...)") "when the extended class has a constructor")
   (page-item "Use" (java "super.method(...)") "to chain to an overridden method"))
  
  )
