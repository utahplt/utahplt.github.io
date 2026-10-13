#cs
(module lecture36 (lib "run.ss" "slideshow")
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
	   (lib "class.ss")
           (lib "etc.ss"))

  (define outline
    (make-outline
     'num "Numbers" #f
     'this "Implicit This" #f
     'static "Static Methods and Fields" #f
     'package "Packages" #f
     'public "Access Modifiers" #f
     'main "Main" #f
     'overload "Overloading" #f
     'exn "Exceptions" #f
     'etc "Etc." #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'num)
  
  (slide/title
   "Integers"
   (page-para "Java's number system:")
   (page-item (java "byte") sym:emdash "an integer between" (java "-128")
              "and" (java "127"))
   (page-item (java "short") sym:emdash "an integer between" (java "-32768")
              "and" (java "32767"))
   (page-item (java "int") sym:emdash "an integer between" (java "-2147483648")
              "and" (java "2147483647"))
   (page-item (java "long") sym:emdash "an integer between" (java "-9223372036854775808")
              "and" (java "9223372036854775807"))
   (blank)
   (page-para "Each set of numbers forms a ring with its operations:")
   (vl-append
    line-sep
    (java "Byte b = 127;")
    (page-para* (java "b + 1") (blank) sym:rightarrow (blank) (java "-128"))
    (page-para* (java "b + 2") (blank) sym:rightarrow (blank) (java "-127"))
    (page-para* (java "b + 2 - 3") (blank) sym:rightarrow (blank) (java "126"))))
  
  (define super-scr
    (opt-lambda (s [extra-style null])
      (text s `(,@extra-style superscript . ,main-font) (current-font-size))))

  (define (expt-str base exp)
    (hbl-append (java base) (colorize sym:times literal-color) (java "10") (super-scr exp)))
  
  (slide/title/tall
   "Floating-Point Numbers"
   (page-item (java "float") sym:emdash "an inexact real between" (expt-str "-3.4" "38")
              "and" (expt-str "3.4" "38"))
   (vl-append
    line-sep
    (page-subitem "Smallest non-zero number:" (expt-str "-1.4" "-45"))
    (page-subitem "Digits of precision: about 6"))
   (page-item (java "double") sym:emdash "an inexact real between" (expt-str "-1.79" "308")
              "and" (expt-str "1.79" "308"))
   (vl-append
    line-sep
    (page-subitem "Smallest non-zero number:" (expt-str "4.94" "-324"))
    (page-subitem "Digits of precision: about 15"))
   (blank)
   (page-para "Addition overflows to a special \"infinity\" value, "
              "and sometimes loses precision:")
   (vl-append
    line-sep
    (java "double d = 5;")
    (page-para* (java "d + 0.0000000000000001") (blank) sym:rightarrow (blank) (java "5.0"))
    (page-para* (java "d + 1e100") (blank) sym:rightarrow (blank) (java "1e100"))))
  
  (slide/title
   "Numbers as Objects"
   (page-para "An" (java "int") "cannot be used as an" (java "Object"))
   (page-para "But Java provides pre-defined classes" (java "Integer") ","
              (java "Double") ", etc.")
   (blank)
   (scale/improve-new-text
    (vl-append
     line-sep
     (java "Object o = new Integer(5);")
     (page-para* (java "o.intValue()") (blank) sym:rightarrow (blank) (java "5"))
     (java " "
           "List l = new Cons(new Integer(7), new Empty());")
     (page-para* (java "((Integer)l.nth(0)).intValue()") (blank) sym:rightarrow (blank) (java "7")))
    0.9))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'this)
  
  (slide/title
   "Implcit This"
   (page-para "In a method," (java "this.") "is addeded implicitly to the"
              "front of field uses and method calls that would be undefined otherwise")
   'alts
   (let ([mk (lambda (c1 c2)
               (java "class Car {"
                     "  String make;"
                     (format "  Car(String ~a) {" c1)
                     (format "    ~amake = ~a;" c2 c1)
                     "  }"
                     "  boolean isSame(Car c) {"
                     "    return make.equals(c.make);"
                     "  }"
                     "  boolean isFord() {"
                     "    isSame(new Car(\"Ford\"));"
                     "  }"
                     "}"))])
     (list (list (mk "make" "this."))
           (list (mk "m" "")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'static)
  
  (slide/title
   "Static Methods"
   (page-para "It's possible to define a function (as opposed to a method)"
              "by using the keyword" (java "static"))
   (vl-append
    line-sep
    (java "class Anything {"
          "  static boolean biggerThanFive(int n) {"
          "     return n > 5;"
          "  }"
          "}"
          " ")
    (page-para* (java "Anything.biggerThanFive(10)") sym:rightarrow (java "true")))
   (blank)
   (page-item "Statics have to be in a class, but you don't have to use" (java "new"))
   (page-item "The function name is prefixed by the class where it's declared")
   (page-item "You can't use" (java "this") "in a static method" sym:emdash "there's no"
              "implicit argument"))
   

  (slide/title
   "Static Fields"
   (page-para "A" (java "static") "field is like a"
              "top-level definition")
   (vl-append
    line-sep
    (java "class Anything {"
          "  static int n = 12;"
          " }"
          " ")
    (page-para* (java "17 + Anything.n") sym:rightarrow (java "29"))
    (java "Anything.n = 15;")))
  
  (slide/title
   "Final Fields"
   (page-para "A" (java "final") "field is like a"
              "constant definition")
   (vl-append
    line-sep
    (java "class Anything {"
          "  final int n = 12;"
          "}"
          " ")
    (page-para* (java "17 + Anything.n") sym:rightarrow (java "29"))
    (tt " ")
    (page-para* (java "Anything.n = 15;") (colorize (it "not allowed") RedColor))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'package)
  
  (slide/title
   "Packages"
   (page-para "Every class declaration resides in a" (dt "package"))
   (page-item "Roughly, the source files in the same directory are all in a package by default")
   (page-item "The" (java "package") "keyword declares the following class"
              "files to be in a particular package")
   (java "package org.plt.lists;"
         " "
         "abstract class List {"
         " ..."
         "}"
         "..."))
  
  (slide/title
   "Using Packages"
   (page-para "To use classes/function from another package, you can prefix"
              "the name with the package name")
   (java "class Anything {"
         "  static boolean biggerThanFive(int n) {"
         "     return n > java.lang.Math.sqrt(25);"
         "  }"
         "}"))
  
  (slide/title
   "Importing Packages"
   (page-para "You can use" (java "import") "to have a prefix applied"
              "to any name that would otherwise be undefined")
   (java "import java.lang;"
         " "
         "class Anything {"
         "  static boolean biggerThanFive(int n) {"
         "     return n > Math.sqrt(25);"
         "  }"
         "}"))

  (slide/title
   "Importing Packages"
   (page-para "Actually," (java "java.lang") "is always imported automatically")
   (java "class Anything {"
         "  static boolean biggerThanFive(int n) {"
         "     return n > Math.sqrt(25);"
         "  }"
         "}")
   (blank)
   (page-para (java "java.lang") "is where" (java "Object") "," (java "Integer") ", etc. come from"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'public)

  (slide/title
   "Access Control"
   (page-para "By default, anything you declare can be seen by anything"
              "in the same package, and only by things in the same page")
   (blank)
   (page-para "For example, this library is probably useless, since no"
              "one outside the package can see" (java "List") ":")
   (java "package org.plt.lists;"
         " "
         "abstract class List {"
         " ..."
         "}"
         "..."))
  
  (slide/title
   "Public Access"
   (page-para "The" (java "public") "modifier makes a declaration"
              "visible to everyone")
   (java "package org.plt.lists;"
         " "
         "public abstract class List {"
         " ..."
         "}"
         "..."))

  (slide/title
   "Method Access"
   (page-para "Modifiers like" (java "public") "must be used on"
              "methods and constructors, too,"
              "to make them visible")
   (java "package org.plt.lists;"
         " "
         "public abstract class List {"
         "  public abstract int length();"
         "  abstract boolean myHelper();"
         "}"
         "public class Empty extends List {"
         "  public Empty() { }"
         "  public int length() { return 0; }"
         "  boolean myHelper() { ... }"
         "}"
         "..."))

  (slide/title
   "Private Access"
   (page-para "The" (java "private") "modifier restricts access"
              "to the current class")
   (java "public class Cons extends List {"
         "  private Object first;"
         "  private List rest;"
         "  public Cons(Object f, List r) {"
         "    first = f; rest = r;"
         "  }"
         "  public Object getFirst() { return first; }"
         "  ..."
         "}")
   (vl-append
    line-sep
    (java "List l = new Cons(new Integer(7), ...);")
    (page-para* (java "((Cons)l).first") (blank) (blank) (colorize (it "not allowed") RedColor))
    (page-para* (java "((Cons)l).getFirst()") (blank) sym:rightarrow (blank) (java "Integer(val = 7)"))))
  
  (slide/title
   "Protected Access"
   (page-para "The" (java "protected") "modifier allows access only within the class or subclasses"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'main)
  
  (slide/title
   "Main"
   (page-para "As you may have noticed, few people actually"
              "run Java programs in ProfessorJ...")
   (page-para "A standalone program is started by calling the"
              (java "Main") "function of a designated class")
   (scale/improve-new-text
    (java "class MyProgram {"
          "  public static void main(String[] cmdLineArgs) {"
          "    ..."
          "  }"
          "}")
    0.8))
                                                    

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'overload)
  
  (slide/title
   "Overloading"
   (page-para "When you change the input part of a method's contract, you"
              "effectively change the name of the method:")
   (scale/improve-new-text
    (vl-append
     line-sep
     (java "class Anything {"
           "  int x;"
           "  boolean y;"
           "  Anything(int x, boolean y) { this.x = x; this.y = y;}"
           "  boolean isSame(int v) { return v == this.x; }"
           "  boolean isSame(boolean v) { return v == this.y; }"
           "}"
           " ")
     (page-para* (java "new Anything(1, false).isSame(1)") sym:rightarrow (java "true"))
     (page-para* (java "new Anything(1, false).isSame(false)") sym:rightarrow (java "true"))
     (page-para* (java "new Anything(1, false).isSame(17)") sym:rightarrow (java "false"))
     (tt " ")
     (page-para* (java "new Anything(1, false).isSame(\"hi\")") (blank)
                 (colorize (it "no such method") RedColor)))
    0.8))

  (slide/title/tall
   "Overloading Can Be Tricky"
   (scale/improve-new-text
    (java "class Car {"
          "  String make;"
          "  Car(String make) { this.make = make; }"
          "  boolean isSame(Car c) { return make.equals(c.make); }"
          "}"
          "class Ford extends Car {"
          "  String model;"
          "  Ford(String model) { super(\"Ford\"); this.model = model; }"
          "  boolean isSame(Ford c) {"
          "    return (make.equals(c.make) && model.equals(c.model));"
          "  }"
          "}"
          " "
          "Car c1 = new Ford(\"Pinto\");"
          "Car c2 = new Ford(\"Fiesta\");")
    0.8)
   (blank)
   'alts
   (list (list (page-para* (java "c1.isSame(c2)") (blank) sym:rightarrow (blank) (java "true"))
               (page-para/r "uses" (java "boolean isSame(Car c)")))
         (list (page-para* (java "((Ford)c1).isSame(c2)") (blank) sym:rightarrow (blank) (java "true"))
               (page-para/r "uses" (java "boolean isSame(Car c)")))
         (list (page-para* (java "c1.isSame((Ford)c2)") (blank) sym:rightarrow (blank) (java "true"))
               (page-para/r "uses" (java "boolean isSame(Car c)")))
         (list
          (page-para* (java "((Ford)c1).isSame((Ford)c2)") (blank) sym:rightarrow (blank) (java "false"))
          (page-para/r "uses" (java "boolean isSame(Ford c)")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'exn)
  
  (slide/title/tall
   "Exceptions"
   (page-para "The" (java "throw") "form is similar to Scheme's"
              (code error))
   (page-para "When a method can raise an exception, the exception must be"
              "declared (usually)")
   (scale/improve-new-text
    (java "abstract class List { ..."
          "  abstract Object nth(int n) throws NthException;"
          "}"
          "class Empty extends List { ..."
          "  Object nth(int n) throws NthException {"
          "    throw NthException(\"index too big\");"
          "  }"
          "}"
          "class Cons extends List { ..."
          "  Object nth(int n) throws NthException {"
          "    if (n == 0)"
          "      return first;"
          "    else"
          "      return rest.nth(n - 1);"
          "  }"
          "}")
    0.75))
  
  (slide/title
   "Catching Exceptions"
   (page-para "The" (java "try ... catch") "form is used to catch"
              "an exception and continue evaluation")
   (java "class Anything {"
         "  static Object nthOrFalse(List l, int n) {"
         "    try {"
         "      return l.nth(n);"
         "    } catch (NthException e) {"
         "      return new Boolean(false);"
         "    }"
         "  }"
         "}"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'etc)
  
  (slide/title
   "A Few More Language Contructs"
   (page-item (java "switch") sym:emdash "a shortcut for" (java "if . . . else if . . ."))
   (page-item "threads" sym:emdash "doing multiple things at a time")
   (page-item "characters" sym:emdash "more than 255!")
   (page-item "implicit string conversion" 
              sym:emdash (java "\"hello\" + 1") sym:rightarrow (java "\"hello1\""))
   (page-item "...")
   (colorize (page-para* "but not too many other things") BlueColor))
  
  (slide/title
   "Lots More Libraries"
   (page-item "GUIs (Swing)")
   (page-item "Container classes (lists, vectors, tables, etc.)")
   (page-item "Big numbers")
   (page-item "Stream I/O")
   (page-item "Cryptography")
   (page-item "Networking")
   (page-item "... ... ... ...")
   (colorize (page-para* "many standard libraries, many more available") BlueColor))
  
  )
