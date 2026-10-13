#cs
(module lecture37 (lib "run.ss" "slideshow")
  (require "utils/colors.ss"
           "utils/code.ss"
	   "utils/java.ss"
           "utils/run.ss"
           "utils/obj.ss"
           "utils/explain.ss"
           "utils/recipe.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           (lib "step.ss" "slideshow")
	   (lib "list.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "etc.ss")
           (lib "math.ss"))

  (slide/title/center
   "This Course was About..."
   (page-para "Fundamentals of programming")
   (page-subitem "From specification to implementation")
   (page-subitem "Software engineering principles"))

  (slide/title/center
   "This Course was..."
   (page-para (bit "Not") "about...")
   (page-subitem "A particular programming language (e.g., Java, C++, Scheme)")
   (page-subitem "A particular programming tool (e.g., gcc, DrScheme)")
   (page-subitem "Specific libraries or protocols (e.g., Gtk, XML, HTTP)")
   (page-subitem "How programs get translated into electronic signals"))
  
  (define (examples . l)
    (apply vl-append gap-size l))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define theme-1 "Theme 1: Data Structures")
  
  (slide/title
   theme-1
   (page-para "Atomic data")
   (examples (code num) (code 1) (blank)
             (code string) (code "apple")))
  
  (slide/title
   theme-1
   (page-para "Compound data")
   (examples (code (code:comment "A posn is")
                   (code:comment " (make-posn num num)"))
             (code (make-posn 1 2)) 
             (blank)
             (java "class Snake {"
                   "  String name;"
                   "  double weight;"
                   "  String food;"
                   "  ..."
                   "}")
             (java "new Snake(\"slinky\", 10, \"rats\")")))
  
  (define lon-defn
    (code (code:comment "A list-of-num is either")
          (code:comment " - empty")
          (code:comment " - (cons num list-of-num)")))
  
  (slide/title
   theme-1
   (page-para "Inductively defined data")
   (page-item "Lists")
   'alts
   (list
    (list
     (examples lon-defn
               (code (cons 1 (cons 2 empty)))))
    (list
     (examples (java "abstract class Pizza { ... }"
                     "class Crust extends Pizza {"
                     "  boolean wheat;"
                     "  ..."
                     "}"
                     "class topping extends Pizza {"
                     "  String top;"
                     "  Pizza bottom;"
                     "  ..."
                     "}")
               (blank)
               (java "new Topping(\"tomato\", 2, new Crust(false))")))))
   
  (slide/title
   theme-1
   (page-item "Trees")
   (scale/improve-new-text
    (examples (code (code:comment "A rumor-mill is either")
                    (code:comment " - empty")
                    (code:comment " - (make-gossip string rumor-mill rumor-mill)"))
              (code (make-gossip \"Amir\"
                                 (make-gossip \"Joe\"
                                              empty 
                                              empty)
                                 (make-gossip \"Linsey\" 
                                              empty 
                                              empty))))
    0.9))
  
  (define dir-defn
    (scale/improve-new-text
     (code (code:comment "A dir is")
           (code:comment " (make-dir sym lofd)")
           code:blank
           (code:comment "A file is")
           (code:comment " (make-file sym num)")
           code:blank
           (code:comment "A lofd is either")
           (code:comment " - empty")
           (code:comment " - (cons file lofd)")
           (code:comment " - (cons dir lofd)"))
     0.8))
  
  (define maze-defn
    (scale/improve-new-text
     (java "class Room {"
           "  Door left;"
           "  Door right;"
           "  ... }"
           "abstract class Door { ... }"
           "class Escape extends Door { ... }"
           "class Into extends Door {"
           "  Room next;"
           "  ..."
           "}"
           "...")
     0.85))
  
  (slide/title
   theme-1
   (page-item "And more:")
   'alts
   (list
    (list
      (examples
       dir-defn
       (scale/improve-new-text
        (code (make-dir 'tmp
                        (list (make-file 'preview.ps 10)
                              (make-dir 'build
                                        (list
                                         (make-file 'x.c 30)
                                         (make-file 'a.out 10))))))
        0.8)))
    (list
      (examples
       maze-defn
       (scale/improve-new-text
        (java "new Into(new Room(new Escape(\"mars\"),"
              "                  new Escape(\"venus\")))")
        0.85)))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define theme-2 "Theme 2: Data Drives Design")
  
  (design-recipe-X theme-2 3 'template)
  
  (slide/title/tall
   theme-2
   'alts
   (list
    (list
     (page-para "The template is a pivotal implementation step:")
     (page-item "Programs that match the shape of the data tend to work, and"
                "they can be understood by others")
     (page-item "Programs that do not match the shape of the data tend to fail"
                "in incomprehensible ways")
     'next
     (blank)
     (vl-append
      gap-size
      (code (code:comment "A list-of-num is either")
            (code:comment " - empty")
            (code:comment " - (cons num list-of-num)"))
      (code (code:contract func : list-of-num -> ...)
            (define (func l)
              (cond
                [(empty? l) ...]
                [else (first l) ... (func (rest l)) ...])))))
    (list (ht-append
           (* 3 gap-size)
           dir-defn
           (scale/improve-new-text
            (code (code:contract dir-func : dir -> ...)
                  (define (dir-func d)
                    ... (dir-name d) 
                    ... (lofd-func (dir-content d)) ...)
                  code:blank
                  (code:contract file-func : file -> ...)
                  (define (file-func f)
                    ... (file-name f) ... (file-size f))
                  code:blank
                  (code:contract lofd-func : lofd -> ...)
                  (define (lofd-func l)
                    (cond
                      [(empty? l) ...]
                      [(file? (first l)) 
                       ... (file-func (first l))
                       ... (lofd-func (rest l))]
                      [(dir? (first l)) 
                       ... (dir-func (first l))
                       ... (lofd-func (rest l))])))
            0.7)))))
                
                
    
               
  

  (slide/title/tall
   theme-2
   (scale/improve-new-text
    (java "class Room {"
          "  Door left;"
          "  Door right; ..."
          "  Path escapePath(Person p) {"
          "    ... left.escapePath(p)"
          "    ... right.escapePath(p) ..."
          "  }"
          "}"
          " "
          "abstract class Door {"
          "  abstract Path escapePath(Person p);"
          "}"
          "class Escape extends Door { ..."
          "  Path escapePath(Person p) { ... }"
          "}"
          "class Into extends Door {"
          "  Room next; ..."
          "  Path escapePath(Person p) {"
          "    ... next.escapePath(p) ..."
          "  }"
          "}")
    0.8))
  
  (slide/title/center
   theme-2
   (colorize
    (page-para* "Good Java style essentially forces you to follow the template")
    BlueColor)
   'next
   (colorize
    (page-para* "Following the template essentially forces good Java style")
    BlueColor))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (define theme-3 "Theme 3: Contracts")
  
  (slide/title
   theme-3
   (page-para "A contract specifies," (it "in advance"))
   (page-item "Obligations of a producer")
   (page-item "Restrictions for a consumer")
   (blank)
   (code (code:contract disk-usage : dir -> num))
   (blank)
   (blank)
   'next
   'alts
   (list (list
          (code (define (disk-usage d)
                  (foldr (lambda (f n)
                           (+ n (file-size f)))
                         0 
                         (dir-content d))))
          (page-para "Producer error:" (code disk-usage) "should work on any" (code dir)))
         (list
          (code ...
                (disk-usage (make-snake 'Slinky 10 'rats)))
          (page-para "Consumer error:" (code disk-usage) "accepts only" 
                     (hbl-append (code dir) (t "s"))))))
  
  (slide/title
   theme-3
   (page-para "A contract identifies the relevant data definition")
   (page-subitem "for examples")
   (page-subitem "for the implementation (template)")
   (page-subitem "for testing" sym:emdash "helps ensure coverage")
   'alts
   (list
    (list
     (code (code:contract disk-usage : dir -> num)
           (define (disk-usage d)
             ... (dir-name d)
             ... (lofd-usage (dir-content d)) ...)
           ...
           (disk-usage (make-dir 'home empty))
           "should be" 0))
    (list
     (blank)
     (blank)
     (colorize
      (with-font 
       `(italic . ,main-font)
       (lambda ()
         (page-para* "Incorrect and abused contracts were the primary source of"
                     "homework difficulties")))
      RedColor))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (define theme-4 "Theme 4: Reuse")

  ;; Copied from lecture 17:
  (define down-arrow
    (colorize (arrow gap-size (* -1/2 pi)) GreenColor))
  (define (howto-step txt)
    (frame (inset (t txt) (/ gap-size 2))))
  (define (hts-append sep . l)
    (let ([g (ghost (apply cc-superimpose l))])
      (apply ht-append sep (map (lambda (i) (cc-superimpose g i)) l))))
  (define rep-step (howto-step "Data Representation and Contract"))
  (define example-step (howto-step "Examples"))
  (define template-branch
    (vc-append
     gap-size
     (howto-step "Template")
     down-arrow
     (howto-step "Body")))
  (define reuse-branch
    (vc-append
     gap-size
     (howto-step "Maybe Abstract")
     down-arrow
     (howto-step "Use Existing")))
  (define genrec-branch
    (vc-append
     gap-size
     (howto-step "Trivial Cases")
     down-arrow
     (howto-step "Recur on Smaller")))
  (define decision-figure
    (vc-append
     gap-size
     rep-step
     down-arrow
     example-step
     (hc-append (* 6 gap-size)
                (colorize (arrow gap-size (* -3/4 pi)) GreenColor)
                down-arrow
                (colorize (arrow gap-size (* -1/4 pi)) GreenColor))
     (hts-append
      (* 2 gap-size)
      reuse-branch
      template-branch
      genrec-branch)
     (hc-append (* 6 gap-size)
                (colorize (arrow gap-size (* -1/4 pi)) GreenColor)
                down-arrow
                (colorize (arrow gap-size (* -3/4 pi)) GreenColor))
     (howto-step "Test")))

  (slide/title/center
   theme-4
   (vl-append
    line-sep
    (page-para "Armed with data definitions and templates, you can write"
               "most things from scratch...")
    (page-para/r (bt "...but you shouldn't")))
   'next
   (blank)
   (colorize
    (page-para* "If nothing else, cut and paste (or deja vu) should trigger reuse")
    BlueColor))
  
  (slide/title
   theme-4
   decision-figure)
  
  (slide/title
   theme-4
   (page-para "Reuse from abstraction:")
   (hc-append
    gap-size
    (scale/improve-new-text
     (code (code:contract sum : list-of-num -> num)
           (define (sum l)
             (cond
               [(empty? l) 0]
               [(cons? l)  
                (+ (first l) 
                   (sum (rest l)))]))
           code:blank
           (code:contract product : list-of-num -> num)
           (define (product l)
             (cond
               [(empty? l) 1]
               [(cons? l) 
                (* (first l) 
                   (product (rest l)))])))
     0.6)
    (colorize (arrow font-size 0) GreenColor)
    (scale/improve-new-text
     (code
      (code:contract combine-nums : list-of-num 
                     code:blank (num num -> num) -> num)
      (define (combine-nums l base-n COMB)
        (cond
          [(empty? l) base-n]
          [(cons? l)
           (COMB (first l)  
                 (combine-nums (rest l) 
                               base-n 
                               COMB))]))
      code:blank
      (code:contract sum : list-of-num -> num)
      (define (sum l)
        (combine-nums l 0 +))
      code:blank
      (code:contract product : list-of-num -> num)
      (define (product l)
        (combine-nums l 1 *)))
     0.6)))

  (slide/title
   theme-4
   (page-para "Reuse from existing abstractions:")
   'alts
   (list
    (list
     (code
      (code:contract sum : list-of-num -> num)
      (define (sum l)
        (foldr + l 0))
      code:blank
      (code:contract product : list-of-num -> num)
      (define (product l)
        (foldr * l 1))))
    (list
     (java "int sum(List l) {"
         "  Enumerator e = l.elements();"
         "  int s = 0;"
         "  while (e.hasMoreElements()) {"
         "    Integer i = (Integer)e.nextElement();"
         "    s = s + i.intValue();"
         "  }"
         "  return s;"
         "}"))))

  (slide/title
   theme-4
   (page-para "Reuse by class extension:")
   (scale/improve-new-text
    (java "class Into extends Door {"
          "  ..."
          "  Path escapePath(Person p) {"
          "    return this.next.escapePath(p);"
          "  }"
          "}"
          " "
          "class Short extends Into {"
          "  ..."
          "  Path escapePath(Person p) {"
          "   if (p.height <= this.height)"
          "     return super.escapePath(p);"
          "   else"
          "     return new Fail();"
          "  }"
          "  // everything else is like Into"
          "}")
    0.8))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define theme-5 "Theme 5: Creativity")
  
  (slide/title/center
   theme-5
   (page-para "A good design process focuses your energy on two"
              "deeply creative problems:")
   (page-subitem "choosing and defining a data representation")
   (page-subitem "implementing the body of a function/method"))

  (slide/title
   theme-5
   (page-para "Problem: choose a data definition for mazes")
   'next
   (blank)
   maze-defn)

  (define robby-bm (make-object bitmap% "robby.png"))
  (define beard-bm (make-object bitmap% "plain-beard.gif"))
  (define glasses-bm (make-object bitmap% "groucho-glasses.gif"))

  (define which-parts
    (vl-append
     line-sep
     (page-subitem "Which part was automatic from contracts?")
     (page-subitem "Which part required creativity?")))
  
  (slide/title
   theme-5
   (page-para "Problem: combine images to check for disguises")
   (hc-append (* 2 gap-size)
              (bitmap robby-bm)
              (bitmap beard-bm)
              (bitmap glasses-bm)
              (blank)
              (let* ([w (send robby-bm get-width)]
                     [h (send robby-bm get-height)]
                     [bm (make-object bitmap% w h)]
                     [dc (make-object bitmap-dc% bm)])
                (send dc draw-bitmap robby-bm 0 0)
                (send dc draw-bitmap glasses-bm
                      (/ (- w (send glasses-bm get-width)) 2)
                      (/ (- h (send glasses-bm get-height)) 2)
                      'solid
                      (make-object color% "black")
                      glasses-bm)
                (send dc set-bitmap #f)
                (bitmap bm)))
   'next
   (blank)
   (scale/improve-new-text
    (code
     (code:contract same-person-maybe-disguised? : 
                    image image image image -> bool)
     (define (same-person-maybe-disguised? p p2 g b)
       (or (image=? p p2)
           (wearing-glasses? p p2 g)
           (wearing-beard? p p2 b)
           (image=? p (add-beard (add-glasses p2 g) b)))))
    0.9)
   'next
   (blank)
   which-parts)
  
  (slide/title
   theme-5
   (page-para "Problem: produce an image's negative")
   (hc-append (* 2 gap-size)
              (bitmap robby-bm)
              (let* ([w (send robby-bm get-width)]
                     [h (send robby-bm get-height)]
                     [bm (make-object bitmap% w h)]
                     [dc (make-object bitmap-dc% bm)])
                (send dc draw-bitmap robby-bm 0 0)
                (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
                (send dc set-brush (send the-brush-list find-or-create-brush "black" 'xor))
                (send dc draw-rectangle 0 0 w h)
                (send dc set-bitmap #f)
                (bitmap bm)))   
   'next
   (blank)
   (code
    (code:contract photo-negative : image -> image)
    (define (photo-negative i)
      (color-list->image 
       (negate-colors (image->color-list i)) 
       (image-width i)
       (image-height i))))
   'next
   (blank)
   which-parts)

  (slide/title
   theme-5
   decision-figure)
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define theme-6 "Theme 6: Programming Tools")
  
  (slide/title
   theme-6
   (page-item "Structures")
   (page-item "Functions")
   (page-item "Classes")
   (page-item "Methods")
   (page-item "Contracts in comments and code")
   (page-item "Local declarations")
   (page-item "Assignment")
   (blank)
   (page-item "Computational complexity"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (slide/title
   "Themes in the Final Exam"
   (page-para "Expect the final exam to hit all of these themes:")
   'alts
   (list (map (lambda (s)
                (page-subitem (regexp-replace "Theme [0-9]: " s "")))
              (list theme-1 theme-2 theme-3
                    theme-4 theme-5 theme-6)))
   (blank)
   (page-para/r "More details next time"))

  )
