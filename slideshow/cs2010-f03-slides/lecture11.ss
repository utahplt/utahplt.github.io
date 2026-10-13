#cs
(module lecture11 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/pipe.ss"
           (lib "etc.ss")
	   (lib "math.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (define seiichi (bitmap "seiichi-ando.png"))
  (define mike (bitmap "mike-boyle.png"))
  (define lindsey (bitmap "lindsey-mcaninch.png"))
  (define amir (bitmap "amir-orome.png"))
  (define derrick (bitmap "derrick-zobell.png"))
  (define joseph (bitmap "joseph-niven.png"))

  (define small-joseph (code-align (scale joseph 0.5 0.5)))
  (define small-seiichi (code-align (scale seiichi 0.5 0.5)))
  (define small-mike (code-align (scale mike 0.5 0.5)))
  (define small-lindsey (code-align (scale lindsey 0.5 0.5)))
  (define small-amir (code-align (scale amir 0.5 0.5)))
  (define small-derrick (code-align (scale derrick 0.5 0.5)))

  (define arrow-size (* 3/2 gap-size))

  (define (name i s)
    (let ([t (t s)])
      (vc-append
       line-sep
       (ghost t)
       i
       t)))

  (define mike-mill
    (hc-append
     gap-size
     (name mike "Mike")
     (colorize (arrow arrow-size 0) GreenColor)
     (name joseph "Joe")))

  (define lindsey-mill
    (hc-append
     gap-size
     (name lindsey "Lindsey")
     (colorize (arrow arrow-size 0) GreenColor)
     (name derrick "Derrick")))

  (define amir-mill
    (inset
     (hc-append
      gap-size
      (name amir "Amir")
      (colorize 
       (vc-append
        (* 3 gap-size)
        (arrow arrow-size (* pi 1/4))
        (arrow arrow-size 0)
        (arrow arrow-size (* pi -1/4)))
       GreenColor)
      (vl-append
       lindsey-mill
       (name seiichi "Seiichi")
       mike-mill))
     0 (- (pict-height (t "Lindsey"))) 0 0))

  (slide/title/tall
   "More Realistic Rumor Mill"
   (page-para "Let each gossip talk to any number of people:")
   amir-mill)

  (define log-defn
    (code (code:comment "A list-of-gossip is either")
          (code:comment "  - empty")
          (code:comment "  - (cons gossip list-of-gossip)")))
  (define gossip-defn
    (code (code:comment "A gossip is")
          (code:comment "  (make-gossip image list-of-gossip)")))

  (slide/title
   "Representing Revised Rumor Mills"
   (hc-append
    gap-size
    (scale amir-mill 0.5 0.5)
    (para (/ client-w 2)
          "How do we represent an arbitrary number of gossip connections?"))
   'next
   (vl-append line-sep
              log-defn
              (code code:blank)
              gossip-defn
              (code (define-struct gossip (who nexts)))))
  
  (define rumor-mill-defn
    (vl-append line-sep
               log-defn
               (code code:blank)
               gossip-defn))
  
  (define gp0-rumor-mill-defn
    (add-gp-arrow
     rumor-mill-defn
     5/6 22/60 1/3 8/60))
  
  (define gp1-rumor-mill-defn
    (add-gp-arrow
     gp0-rumor-mill-defn
     1/3 28/60 1/5 42/60 #t))

  (define gp-rumor-mill-defn
    (add-gp-arrow
     gp1-rumor-mill-defn
     3/4 52/60 1/3 8/60))

  (slide/title
   "Programming with Revised Rumor Mills"
   'alts~
   (list (list rumor-mill-defn)
         (list gp0-rumor-mill-defn)
         (list gp1-rumor-mill-defn)
         (list gp-rumor-mill-defn)
         (list (scale gp-rumor-mill-defn 0.75 0.75)))
   (code (define (func-for-log l)
           (cond
             [(empty? l) ...]
             [(cons? l)
              ... (func-for-gossip (first l))
              ... (func-for-log (rest l))]))
         code:blank
         (define (func-for-gossip g)
           ... (gossip-who g) 
           ... (func-for-log (gossip-nexts g)) ...)))
  
  (slide/title
   "Examples for Revised Rumor Mills"
   (problem "Implement" (code count-people) ", which takes a gossip"
            "and returns the number of people informed by the gossip"
            "(including the starting person)")
   (problem "Implement the function" (code informed?) "which takes a person image and a gossip"
	    "and determines whether the person is part of the rumor mill")
   (problem "Implement" (code remove-person) ", which takes a person image and a gossip and"
            "returns a gossip where the given person is uninformed")
   (blank)
   (page-para* "... and any other function for the old rumor mills"))
  
  )
