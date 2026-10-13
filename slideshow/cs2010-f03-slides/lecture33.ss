#cs
(module lecture33 (lib "run.ss" "slideshow")
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
     'obj "Contracts and Abstraction" #f
     'cast "Casts" #f
     'instanceof "Checking a Type" #f
     'intf "Interfaces" #f))
     
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'obj)
  
  (slide/title
   "Contracts"
   (page-para "What is the contract for the" (java "equals") 
              "method of" (java "String") "?")
   (java "\"hello\".equals(...)")
   'next
   (blank)
   (page-para "So far, we've pretended that it takes a" (java "String")
              "and produces a" (java "boolean"))
   (vl-append
    (/ gap-size 2)
    (page-para* (java "\"hello\".equals(\"bye\")") sym:rightarrow (java "false"))
    (page-para* (java "\"hello\".equals(8)")
                (colorize (it "   contract mismatch") RedColor)))
   'next
   (blank)
   (page-para "The truth is somewhat more complex:")
    (page-para* (java "\"hello\".equals(new Posn(1, 2))") sym:rightarrow (java "false")))

  (slide/title
   "The Whole Truth"
   (page-item "The" (java "equals") "method takes an" (java "Object")
              "and returns a" (java "boolean"))
   (page-item "Every class extends" (java "Object"))
   (vl-append gap-size
              (inset
               (scale/improve-new-text
                (java "class Posn {"
                      "  double x;"
                      "  ..."
                      "}")
                0.8)
               gap-size 0 0 0)
              (t "is a shorthand for")
              (inset
               (scale/improve-new-text
                (java "class Posn extends Object {"
                      "  double x;"
                      "  ..."
                      "}")
                0.8)
               gap-size 0 0 0))
   (page-item "The" (java "equals") "method is defined in" (java "Object")))
  
  (slide/title/center
   "The Default Equals Method"
   (java "class Object {"
         "  ..."
         "  boolean equals(Object o) {"
         "    return o == this;"
         "  }"
         "}")
   (page-para/r "where" (java "==") "is like" (code eq?) "in Scheme"))
  
  (slide/title
   "Using Object for Abstraction"
   (page-para "In Scheme, we eventually wrote abstractions for lists:")
   (code (code:comment "A list-of-X is either")
         (code:comment "  - empty")
         (code:comment "  - (cons X list-of-X)"))
   'next
   (blank)
   'alts
   (list (list (page-para "A precise translation to a Java-like notation:")
               (scale/improve-new-text
                (java "abstract class ListOf<X> { }"
                      "class EmptyListOf<X> { ... }"
                      "class ConsListOf<X> {"
                      "  <X> first;"
                      "  ListOf<X> rest;"
                      "  ..."
                      "}"
                      " "
                      "new ConsListOf<String>(\"apple\", ...)")
                0.75)
               'next
               (page-para "But Java doesn't support this," (it "yet")))
         (list (page-para "A usable translation to Java:")
               (scale/improve-new-text
                (java "abstract class List { }"
                      "class Empty { ... }"
                      "class Cons {"
                      "  Object first;"
                      "  List rest;"
                      "  ..."
                      "}"
                      " "
                      "new Cons(\"apple\", ...)")
                0.75))))
  
  (define-values (objlist-code objlist-str)
    (java/copy
     0.8
     "abstract class List {"
     "  abstract boolean isMember(Object o);"
     "}"
     " "
     "class Empty extends List {"
     "  Empty() { }"
     "  boolean isMember(Object o) { return false; }"
     "}"
     " "
     "class Cons extends List {"
     "  Object first;"
     "  List rest;"
     "  Cons(Object first, List rest) {"
     "   this.first = first; this.rest = rest;"
     "  }"
     "  boolean isMember(Object o) {"
     "    return this.first.equals(o) || this.rest.isMember(o);"
     "  }"
     "}"))
  
  (slide/title
   "Object Lists"
   (ct-superimpose
    objlist-code
    (mk-copy objlist-str)))
  
  (slide/title
   "Extracting Objects"
   (problem "Implement the" (java "List") "method"
            (java "nth") ", which takes a number" (it "n")
            "and returns the first item in the list after skipping"
            (it "n") "items, or an empty list if no items are left"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'cast)
  
  (slide/title
   "Using Extracted Objects"
   (scale/improve-new-text
    (vl-append
     line-sep
     (java "new Cons(new Posn(1, 2), new Empty()).nth(0)")
     (page-para* sym:rightarrow (java "Posn(x = 1,y = 2)"))
     (tt " ")
     (java "new Cons(new Posn(1, 2), new Empty()).nth(0).x")
     (colorize (it "contract error") RedColor))
    0.9)
   'next
   (blank)
   (page-para "The contract error occurs becuase" (java "nth")
              "promises merely to return an" (java "Object") ", not necessarily"
              "a" (java "Posn"))
   'next
   (page-para "Java provides a way around this weakness in the contract system..."))
  
  (slide/title
   "Casts"
   (page-para "A" (dt "cast") "is a dynamic request for an improved contract")
   (page-para "General syntax:")
   (java "(_Class)_expr")
   (page-para/r (it "The parentheses are required"))
   'next
   (blank)
   (page-para "Examples:")
   (scale/improve-new-text
    (java "(Posn)(new Cons(new Posn(1, 2), new Empty()).nth(0))"
          " "
          "Path escapePath(Person p) {"
          "  Path lp = this.left.escapePath(p);"
          "  if (lp.isOk())"
          "    return new Left((Success)lp);"
          "  ..."
          "}")
    0.9))

  (define-values (posn-code posn-str)
    (java/copy
     0.9
     "class Posn {"
     "  double x;"
     "  double y;"
     "  Posn(double x, double y) {"
     "    this.x = x; this.y = y;"
     "  }"
     "  boolean equals(Object o) {"
     "    return (this.x == ((Posn)o).x)"
     "           && (this.y == ((Posn)o).y);"
     "  }"
     "}"))

  (slide/title
   "Using A Cast to implement equals"
   (page-para "A problem with" (java "Posn") ":")
   (vl-append (java "new Posn(1, 2).equals(new Posn(1, 2))")
              (page-para* sym:rightarrow (java "false")))
   'next
   (page-para "To fix this, we need to override" (java "equals") ":")
   (cb-superimpose
    posn-code
    (mk-copy posn-str)))

  (define-values (equals-code equals-str)
    (java/copy
     1.0
     "boolean equals(Object o) {"
     "  if (o instanceof Posn)"
     "    return (this.x == ((Posn)o).x)"
     "           && (this.y == ((Posn)o).y);"
     "  else"
     "    return false;"
     "}"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'instanceof)
  
  (slide/title
   "Checking Types"
   (page-para "A remaining problem:")
   (vl-append 
    (/ gap-size 2)
    (page-para* (java "\"hello\".equals(new Posn(1, 2))") sym:rightarrow (java "false"))
    (page-para* (java "new Posn(1, 2).equals(\"hello\")")
                sym:rightarrow (colorize (it "cast failed") RedColor)))
   'next
   (blank)
   (page-para "Our" (java "equals") "should only cast if the argument"
              "really is a" (java "Posn"))
   'next
   (page-para "The" (java "instanceof") "operator tests whether a cast will succeed")
   (cb-superimpose
    equals-code
    (mk-copy equals-str)))
  
  (slide/title
   "Using instanceof"
   (page-para "The" (java "instanceof") "operator is only in"
              (bt "Advanced Java") "because it's rarely the"
              "right way to implement something")
   (page-para "Example bad use:")
   (java "class Cons extends List {"
         "  ..."
         "  boolean isMember(Object o) {"
         "    if (this.first.equals(o))"
         "      return true;"
         "    else if (this.rest instanceof Empty)"
         "      return false;"
         "   ..."
         "  }"
         "}"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'intf)
  
  (slide/title
   "Named Doors"
   (page-para "Suppose that we want to make the following improvements"
              "to our maze game:")
   (page-item "Some doors will have names")
   (page-item "We want to get all of the named places"
              "in a maze, including both escapes and named doors")
   (page-item "We'll need certain methods on named places, such as" (java "isNice"))
   (page-item "We don't want to add named-place methods to all doors")
   (page-item "We refuse to use" (java "instanceof"))
   (blank)
   (scale/improve-new-text
    (java "abstract class Door {"
          "  ..."
          "  abstract List places();"
          "}")
    0.9))
  
  (slide/title
   "A NamedPlace Abstract Class"
   (page-para "Like this?")
   (tree-append
    (mk-class (java "_NamedPlace")
              (list (java "String name"))
              (list (java "boolean _isNice()")))
    (mk-class (java "Escape")
              (list (blank))
              (list (java "boolean isNice()")))
    (mk-class (java "NamedInto")
              (list (blank))
              (list (java "boolean isNice()"))))
   'next
   (blank)
   (page-para (java "NamedPlace") "can't be an abstract class, because" (java "Escape")
              "already extends" (java "Door") ", and"
              (java "NamedInto") "should extend" (java "Into"))
   (colorize (page-para* "A class must extend exactly one class") BlueColor)
   'next
   (page-para "However," (java "NamedPlace") "can be an interface..."))
  
  (slide/title
   "Interface"
   (page-para "An" (dt "interface") "is like an" (java "abstract") (java "class")
              "with no fields and all abstract methods")
   (scale/improve-new-text
    (java "interface NamedPlace {"
          "  boolean isNice();"
          "}")
    0.85)
   'next
   (blank)
   (page-para "Instead of" (hbl-append (tt "extend") (t "ing"))
              "an interface, classes" (tt "implement") "it")
   (scale/improve-new-text
    (java "class Escape extends Door implements NamedPlace {"
          "  ..."
          "  boolean isNice() { return true; }"
          "}"
          " "
          "class NamedInto extends Into implements NamedPlace {"
          " ..."
          "  boolean isNice() { return false; }"
          "}")
    0.85))
   
  (define (sjava . l)
    (scale/improve-new-text (apply java l) 0.8))
  
  (define ... (sjava "..."))
  
  (define escape-class
    (mk-class (sjava "Escape")
              (list (sjava "String name"))
              (list ...)))

  (define named-into-class
    (mk-class (sjava "NamedInto")
              (list (sjava "String name"))
              (list ...)))

  (define named-place-intf
    (itree-append
     (mk-class (sjava "_NamedPlace")
               null
               (list ...))))
  
  (slide/title
   "Door Hierarchy with Interfaces"
   (let ([p (lt-superimpose
             named-place-intf
             (inset
              (tree-append
               (mk-class (sjava "_Door")
                         (list (blank))
                         (list ...))
               escape-class
               (tree-append/keep-root
                #f #t
                (mk-class (sjava "_Into")
                          (list (sjava "Room next"))
                          (list ...))
                named-into-class
                (mk-class (sjava "Short")
                          (list (sjava "double height"))
                          (list ...))
                (mk-class (sjava "Locked")
                          (list (sjava "String keyColor"))
                          (list ...))))
              (* 1 (pict-width named-into-class)) 0 0 0))])
     (let-values ([(ix iy) (find-cb p named-place-intf)]
                  [(ex ey) (find-lt p escape-class)]
                  [(nx ny) (find-lt p named-into-class)])
       (place-over (place-over p ix (- (pict-height p) iy)
                               (itreeline
                                (vr-append
                                 (hline (- nx ix (- gap-size)) 0)
                                 (vline 0 (- iy ny)))))
                   ix (- (pict-height p) iy)
                   (itreeline
                    (vr-append
                     (hline (- ex ix (- gap-size)) 0)
                     (vline 0 (- iy ey))))))))
  (slide/title
   "Single vs. Multiple, Implementation vs. Interface"
   (page-para "A class must extend only one class")
   (page-subitem "This is" (dt "single inheritance") "of" (dt "implementation"))
   (blank)
   (page-para "A class interface can implement any number of interfaces")
   (page-subitem "This is" (dt "multiple inheritance") "of" (dt "interface")))
   
       

  )
