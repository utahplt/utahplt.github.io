#cs
(module lecture27 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
           "utils/run.ss"
           "utils/comm.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
	   (lib "string.ss")
           (lib "list.ss"))

  (define mtr (scale/improve-new-text (bt "Mid-Term 2 Review!") 2))
  
  (slide/center
   (colorize (t "Scary Halloween") "forestgreen")
   (cc-superimpose (colorize (inset mtr 0 3 3 0) "lightgreen")
                   (colorize (inset mtr 5 5 0 0) "green")
                   (colorize mtr "forest green"))
   (jack-o-lantern (* client-w 1/2)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define outline
    (make-outline
     'mt2 "Mid-Term 2 Review" #f
     'java "Intro to Java" #f))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'mt2)
  
  (slide/title/center
   "Mid-Term 2"
   (page-para "Topics covered:")
   (page-item "Program design for atomic data through lists (as in Mid-Term 1)")
   (page-item "Trees and mutually recursive data structures")
   (page-item (code local) ", functions as values, and" (code lambda))
   (page-item "Abstraction for functions and contracts")
   (page-item "Generative recursion and accumulators")
   (page-item "State and assignment"))
  
  (slide/title
   "Mid-Term Style"
   (page-item "In-class, open-book, open-notes, closed-computer")
   (blank)
   (page-item "Fill-in-the-blank")
   (page-subitem "You'll receive a medium-sized program with gaps")
   (page-subitem "Gaps can appear anywhere: data definition, contract, purpose, examples, implementation")
   (blank)
   (page-item "Examples to appear on the web page")
   (page-subitem "Small example in class today")
   (page-subitem "Another example with and without solution"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
  (outline 'java)
  
  (slide/title
   "Java"
   (page-para (dt "Java") "is a programming language that was invented in the 90s")
   (blank)
   (page-para "It's probably the most popular programming language right now")
   (page-subitem "Syntax looks like C/C++")
   (page-subitem "Semantics more like Lisp/Scheme/Smalltalk")
   (blank)
   (page-para "CS 2020 uses Java")
   (page-para "CS 3500 uses C++"))

  (slide/title
   "Java in CS 2010 F03"
   (page-para "Recent semesters of CS 2010 included a quick"
              "tour of Java during the last week of class")
   (page-para "For a variety of reasons (especially the switch to MWF lectures)"
              "we've arrived at Java sooner")
   'next
   (blank)
   (blank)
   (page-para "Furthermore, we'll")
   (page-item "approach Java in a different way than previous semesters")
   (page-item "continue to use DrScheme, in an experimental Java mode")
   (page-item "use an early draft of a HtDP-to-Java textbook"))

  (slide/center
   (bitmap "guinea-pigs.jpg")
   'next
   (t "or")
   (bitmap "armstrong.jpg"))
  
  (slide/center
   (it "RumorMill example in DrScheme/ProfessorJ..."))
   

  )
