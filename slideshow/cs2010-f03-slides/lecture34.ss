#cs
(module lecture34 (lib "run.ss" "slideshow")
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
     'abstraction "Abstraction" #f
     'state "State" #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'abstraction)
  
  (define-values (append-code append-str)
    (java/copy
     0.8
     "abstract class List {"
     "  abstract List appendAll(String s);"
     "}"
     " "
     "class Empty extends List {"
     "  Empty() { }"
     "  List appendAll(String s) { return new Empty (); }"
     "}"
     " "
     "class Cons extends List {"
     "  Object first;"
     "  List rest;"
     "  Cons(Object first, List rest) {"
     "   this.first = first; this.rest = rest;"
     "  }"
     "  List appendAll(String s) {"
     "    return new Cons(((String)this.first).concat(s),"
     "                    this.rest.appendAll(s));"
     "  }"
     "}"))
  
  (slide/title
   "List Maps: Append to Each"
   (cb-superimpose
    append-code
    (mk-copy append-str)))

  (define (combo . l)
    (define-values (code str)
      (apply java/copy 0.8 l))
    (hbl-append gap-size code (mk-copy* str)))
    
  (define (sjava . l)
    (scale/improve-new-text (apply java l) 0.8))
  
  (define (map-slide title decl empty cons)
    (slide/title
     title
     (vl-append
      line-sep
      (sjava
       "abstract class List {"
       "  ...")
      (combo
       decl)
      (sjava
       "}"
       " "
       "class Empty extends List {"
       "  ...")
      empty
      (sjava
       "}"
       " "
       "class Cons extends List {"
       "  ...")
      cons
      (sjava
       "}"))))
  
  (map-slide "List Maps: Prefix to Each"
             "  abstract List prefixAll(String s);"
             (combo
              "  List prefixAll(String s) { return new Empty (); }")
             (combo
              "  List prefixAll(String s) {"
              "    return new Cons(s.concat((String)this.first),"
              "                    this.rest.prefixAll(s));"
              "  }"))

  (map-slide "List Maps: Upcasing Each"
             "  abstract List upAll();"
             (combo
              "  List upAll() { return new Empty (); }")
             (combo
              "  List upAll() {"
              "    return new Cons(((String)this.first).toUpperCase(),"
              "                    this.rest.upAll());"
              "  }"))

  (map-slide "List Maps: Trimming Each"
             "  abstract List trimAll();"
             (combo
              "  List trimAll() { return new Empty (); }")
             (combo
              "  List trimAll() {"
              "    return new Cons(((String)this.first).trim(),"
              "                    this.rest.trimAll());"
              "  }"))

  (slide/title
   "List Maps"
   (page-para "Every time we write a map method, we mostly repeat work:")
   (page-item "Declare an abstract method")
   (page-item "Implement the method in" (java "Empty") "to return" (java "new Empty()"))
   (page-item "Implement the method in" (java "Cons") ":")
   (page-subitem "Do something to" (java "this.first"))
   (page-subitem "Recursively call method of" (java "this.rest"))
   (page-subitem "Combine with" (java "new Cons(...)"))
   'next
   (blank)
   (colorize
    (page-para* "Can we abstract all of this work?")
    BlueColor))
  
  (define-values (map-code map-str)
    (java/copy
     0.8
     "interface Xformer { Object xform(Object o); }"
     " "
     "abstract class List {"
     "  abstract List map(Xformer x);"
     "}"
     " "
     "class Empty extends List {"
     "  Empty() { }"
     "  List map(Xformer x) { return new Empty (); }"
     "}"
     " "
     "class Cons extends List {"
     "  Object first; List rest;"
     "  Cons(Object first, List rest) {"
     "   this.first = first; this.rest = rest;"
     "  }"
     "  List map(Xformer x) {"
     "    return new Cons(x.xform(this.first),"
     "                    this.rest.map(x));"
     "  }"
     "}"))
  
  (slide/title/tall
   "Generic List Map"
   (cb-superimpose
    map-code
    (mk-copy map-str)))

  (define-values (append-xform-code append-xform-str)
    (java/copy
     0.8
     "class Append implements Xformer {"
     "  String s;"
     "  Append(String s) { this.s = s; }"
     "  Object xform(Object o) { "
     "    return ((String)o).concat(this.s);"
     "  }"
     "}"))
  
  (define-values (upcase-xform-code upcase-xform-str)
    (java/copy
     0.8
     "class Upcase implements Xformer {"
     "  Upcase() { }"
     "  Object xform(Object o) { "
     "    return ((String)o).toUpperCase();"
     "  }"
     "}"))

  (with-steps~
   (a b)
   (slide/title/tall
    "Using the Generic List Map"
    (vl-append
     gap-size
     (hbl-append
      append-xform-code
      (mk-copy* append-xform-str))
     (scale/improve-new-text
      (java
       "List l = new Cons(\"a\", new Cons(\"b\", new Empty()));"
       "l.map(new Append(\"x\"))")
      0.8)
     (blank)
     ((vafter b)
      (hbl-append
       upcase-xform-code
       (mk-copy* upcase-xform-str)))
     (blank)
     ((vafter b)
      (scale/improve-new-text
       (java
        "l.map(new Upper())")
       0.8)))))
  
  (slide/title
   "Anonymous Classes"
   (page-para "In full Java," (dt "anonymous classes") "make abstraction easier,"
              "just like" (code lambda) ":")
   (java "l.map(new Xformer() {"
         "        Object xform(Object o) {"
         "           return ((String)o).toUpperCase();"
         "        }"
         "      })"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'state)

  (define-values (fish-code fish-str) 
    (java/copy
     0.9
     "class Fish {"
     "  double weight;"
     "  Fish(double weight) {"
     "    this.weight = weight;"
     "  }"
     "  double getWeight() {"
     "    return this.weight;"
     "  }"
     "  void feed(double n) {"
     "    this.weight = this.weight + n;"
     "  }"
     "}"))

  (slide/title
   "State"
   (page-para "Java objects encapsulate their fields, and" (java "=")
              "assigns to a field (in" (bt "Advanced Java") "and full Java)")
   (cb-superimpose
    fish-code
    (mk-copy fish-str))
   (blank)
   (colorize
    (page-para* "Note: no" (java "return") "for a" (java "void") "method")
    BlueColor))
  
  (slide/title/center
   "State Examples"
   (vl-append
    line-sep
    (java
     "Fish alice = new Fish(7);"
     "Fish bob = new Fish(6);")
    (tt " ")
    (page-para* (java "alice.getWeight()") sym:rightarrow (java "7"))
    (page-para* (java "bob.getWeight()") sym:rightarrow (java "6"))
    (tt " ")
    (page-para* (java "alice.feed(3)"))
    (tt " ")
    (page-para* (java "alice.getWeight()") sym:rightarrow (java "10"))
    (page-para* (java "bob.getWeight()") sym:rightarrow (java "6"))))

  (define-values (aq-code aq-str)
    (java/copy
     0.8
     "class Aq {"
     "  List fishes;"
     "  int count;"
     "  Aq() {"
     "    this.fishes = new Empty();"
     "    this.count = 0;"
     "  }"
     "  void add(Fish f) {"
     "    this.fishes = new Cons(f, this.fishes);"
     "    this.count = this.count + 1;"
     "  }"
     "  void feedAll(int n) {"
     "    this.fishes.map(new Feeder(n));"
     "  }"
     "}"))  

  (define-values (feeder-code feeder-str)
    (java/copy
     0.8
     "class Feeder implements Xformer {"
     "  int n;"
     "  Feeder(int n) { this.n = n; }"
     "  Object xform(Object o) { "
     "    ((Fish)o).feed(this.n);"
     "    return this; // result will be ignored, anyway"
     "  }"
     "}"))

  (slide/title
   "Objects that Contain Lists"
   (page-para "Use the constructor to initialize state, even without arguments:")
   (cb-superimpose
    aq-code
    (mk-copy aq-str))
   (blank)
   (colorize
    (page-para* "Note:" (code begin)
                "is implicit")
    BlueColor))
  
  (slide/title/center
   "Feeder"
   (cb-superimpose
    feeder-code
    (mk-copy feeder-str)))
  
  (define-values (color-fish-code color-fish-str)
    (java/copy
     0.8
     "class ColorFish extends Fish {"
     "  String color;"
     "  ColorFish(double weight, String color) {"
     "    super(weight);"
     "    this.color = color;"
     "  }"
     "}"))
  
  (slide/title
   "State and Abstraction"
   (page-para "Of course, we can put colorful fish in our aquarium:")
   (cb-superimpose
    color-fish-code
    (mk-copy color-fish-str))
   'next
   (blank)
   (scale/improve-new-text
    (vl-append
     line-sep
     (java
      "Aq a = new Aq();"
      "a.add(new Fish(10))"
      "a.add(new ColorFish(11, \"blue\"))"
      "a.feedAll(3)")
     (htl-append (java "a") (t "  ") sym:rightarrow (t "  ")
                 (java "Aq(fishes = Cons(first = ColorFish(weight = 14,"
                       "                                   color = \"blue\"),"
                       "                 rest = Cons(first = Fish(weight = 13),"
                       "                             rest = Empty())),"
                       "   count = 2)"))
     (tt " ")
     (page-para* (java "a.add(\"hello\")") sym:rightarrow (colorize (it "contract error") RedColor)))
    0.6))
  
  (slide/title
   "Arrays"
   (page-para "Java arrays are like Scheme vectors, except that the"
              "contract for the array elements is explicit")
   (page-item "The type of an array of" (java "_X") "is" (java "_X[]"))
   (page-item "To make a" (java "_X[]") "with" (java "_n") "elements:"
              (java "new _X[_n]"))
   (page-item "If" (java "_x") "is an array, then")
   (page-subitem (java "_x[_n]") "gets its" (hbl-append (java "_n") (t "th")) "element")
   (page-subitem (java "_x[_n] = _o") "sets its" (hbl-append (java "_n") (t "th")) "element to" (java "_o"))
   (blank)
   (vl-append
    line-sep
    (java "Fish[] v = new Fish[10];"
          "v[0] = new Fish(2);"
          "v[0].feed(4);")
    (page-para* (java "v[0]") (blank) sym:rightarrow (blank) (java "Fish(weight = 6)"))))

  (slide/title
   "null"
   (page-para "What about" (java "v[1]") "through" (java "v[9]") "?")
   'next
   (blank)
   (page-item "Java includes a built-in constant" (java "null")
              "that can act as any object type")
   (page-item "Arrays are initialized to have" (java "null")
              "as all elements")
   (blank)
   (blank)
   (page-para* (java "v[4]") (blank) sym:rightarrow (blank) (java "null"))
   (page-para* (java "v[4].feed(1)") (blank) sym:rightarrow (blank) (colorize (it "illegal use of null") RedColor))
   (blank)
   (blank)
   (colorize
    (page-para* "Note that the last example is" (it "not")
                "a contract error")
    BlueColor))
                                                            
  (slide/title
   "Array Contracts"
   (page-para "If you have a" (java "ColorFish") ", you can use it as a"
              (java "Fish"))
   (java "ColorFish charlie = new ColorFish(10, \"blue\");"
         "Fish afish = charlie;")
   'next
   (blank)
   (page-para "If you have an array of" (java "ColorFish") 
              ", can you can use it as an array of"
              (java "Fish") "?")
   'next
   (htl-append (* 1.5 gap-size)
               (bt "Yes:")
               (java "ColorFish[] neons = new ColorFish[10];"
                     "Fish[] fishes = neons;"))
   'next
   (blank)
   'alts
   (list (list (page-para "Good:")
               (vl-append
                line-sep
                (java "fishes[0] = afish; // which is charlie")
                (page-para* (java "fishes[0].getWeight()")
                            (blank) sym:rightarrow (blank)
                            (java "10"))
                (page-para* (java "neons[0].color")
                            (blank) sym:rightarrow (blank)
                            (java "\"blue\""))))
         (list (page-para "Bad:")
               (vl-append
                line-sep
                (java "fishes[0] = new Fish(10);")
                (page-para* (java "neons[0].color")
                            (blank) sym:rightarrow (blank)
                            (it "???")))
               'next
               (page-para "Java therefore disallows the assignment dynamically"))))
  
  (slide/title/center
   "The Effect of State on Contracts"
   (page-item "At run-time, you can get an" 
              (colorize (it "illegal use of null") RedColor)
              "error")
   (page-item "At run-time, you can get an" 
              (colorize (it "illegal array assignment") RedColor)
              "error")
   (blank)
   (blank)
   (page-para "Unlike the problem of using" (java "ListOfObject")
              "intstead of" (java "ListOf<X>") ", these problems"
              "won't go away in future versions of Java"))
   

  )
