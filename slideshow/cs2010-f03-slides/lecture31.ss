#cs
(module lecture31 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/run.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (slide/title
   "Extended Example (Iterative Refinement)"
   (page-para "A maze consists of rooms and doors:")
   (page-item "An door is either")
   (page-subitem "a door into a room")
   (page-subitem "an escape to a particular place")
   (page-item "A room has two doors, left and right"))

  (define-values (exit-code exit-str)
    (java/copy
     0.75
     "abstract class Door {"
     "}"
     " "
     "class Into extends Door {"
     "  Room next;"
     "  Into(Room next) {"
     "    this.next = next;"
     "  }"
     "}"
     " "
     "class Escape extends Door {"
     "  String name;"
     "  Escape(String name) {"
     "    this.name = name;"
     "  }"
     "}"))

  (slide/title
   "Door Data Definition"
   exit-code
   (mk-copy exit-str))

  (define-values (room-code room-str)
    (java/copy
     1
     "class Room {"
     "  Door left;"
     "  Door right;"
     "  Room(Door left, Door right) {"
     "   this.left = left;"
     "   this.right = right;"
     "  }"
     "}"))
  
  (slide/title
   "Room Data Definition"
   room-code
   (mk-copy room-str))

  (define-values (factory-code factory-str)
    (java/copy
     0.9
     "class Factory {"
     "  Factory() { }"
     "  Room Example() {"
     "    Door meadow = new Escape(\"meadow\");"
     "    Door street = new Escape(\"street\");"
     "    Room ms = new Room(meadow, street);"
     "    Room planets = new Room(new Escape(\"mars\"),"
     "                            new Escape(\"venus\"));"
     "    return new Room(new Into(ms),"
     "                    new Into(planets));"
     "  }"
     "}"))

  
  (slide/title
   "Factory for Examples"
   factory-code
   (mk-copy factory-str)
   (page-para* "Local definitions" sym:implies (bt "Intermediate Java")))

  (slide/title
   "Finding Paths"
   (problem "Implement the" (java "Door") "method" (java "canEscape")
            "that takes a string and returns a boolean indicating"
            "whether an escape with the given name is available")
   'next
   (problem "Replace the" (java "canEscape") "method with a" (java "escapePath")
            "method that takes a string and returns either a path of"
            "\"left\" and \"right\" leading to the escape, or a failure"
            "value")
   (java "Path escapePath(String dest)"))
  
  (slide/title
   "Paths"
   (page-para "A path result is either")
   (page-item "failure")
   (page-item "immediate success")
   (page-item "left followed by a (succesful) path")
   (page-item "right followed by a (successful) path")
   'next
   (blank)
   (colorize
    (page-para* "We'll need a" (java "Path") "abstract class with an" (tt "isOk") "method")
    BlueColor))

  (define-values (path-code path-str)
    (java/copy
     0.6
     "abstract class Path {"
     "  abstract boolean isOk();"
     "}"
     " "
     "class Fail extends Path {"
     "  Fail() { }"
     "  boolean isOk() { return false; }"
     "}"
     " "
     "class Success extends Path {"
     "  Success() { }"
     "  boolean isOk() { return true; }"
     "}"
     " "
     "class Right extends Path {"
     "  Path rest;"
     "  Right(Path rest) { this.rest = rest; }"
     "  boolean isOk() { return true; }"
     "}"
     " "
     "class Left extends Path {"
     "  Path rest;"
     "  Left(Path rest) { this.rest = rest; }"
     "  boolean isOk() { return true; }"
     "}"))


  (slide/title
   "Paths"
   (cb-superimpose
    path-code
    (mk-copy path-str)))

  (slide/title
   "Door Variations and Person Attributes"
   (page-para "Eventually, we want locked doors, short doors, magic doors, and other kinds of doors")
   (page-para "Finding an escape will depend on having keys, being a certain height, etc.")
   (page-para "Instead of adding more and more arguments to" (java "escapePath")
              ", let's introduce a" (java "Person") "to carry attributes")
   (blank)
   (problem "Replace the destination-string argument of" (java "escapePath")
            "with a" (java "Person") "argument, where a" (java "Person")
            "has a destination and height"))
  
  (slide/title
   "Short Doors"
   (problem "Add a new kind of door, a short door, where a person must be less"
            "that the door's height to pass")
   'next
   (blank)
   (blank)
   (page-para "Adding a short door requires only the declaration of a" (java "Short")
              "class" sym:emdash "no other code changes!"))
  
  (slide/title
   "Locked Doors"
   (problem "Add a new kind of door, a locked door, where a person must have"
            "a key to pass")
   (blank)
   (blank)
   (page-para "Besides adding" (java "Locked") ", we change" (java "Person")
              "to add the notion of keys to the person")
   (page-para "In contrast to adding new variants, adding new operations"
              "requires changing the class"))
  
  (slide/title
   "Scheme versus Java"
   (page-para "Scheme:")
   (page-subitem "New variant" sym:implies "change old functions")
   (page-subitem "New function" sym:implies "no changes to old code")
   (page-para "Java:")
   (page-subitem "New variant" sym:implies "no changes to old code")
   (page-subitem "New method" sym:implies "change old classes")
   (blank)
   (page-para "This is the essential difference between"
              (dt "functional") "programming and" (dt "object-oriented")
              "programming"))
  
  )
