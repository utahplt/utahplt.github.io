#cs
(module lecture24 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           (all-except "utils/utils.ss" with-steps with-steps~)
           "utils/code.ss"
           "utils/eval-step.ss"
           "utils/run.ss"
           (lib "step.ss" "slideshow")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "math.ss")
           (lib "etc.ss")
           (lib "list.ss"))

  (define outline
    (make-outline
     'obj "Encapsulation and Objects" #f
     'vec "Vectors and Identity" #f))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'obj)
  
  (define-code/string/scale 
   1.0
   (fish-object-run fish-object-code fish-object-string)
   (code:comment "A fish-object is")
   (code:comment "  (num -> num)")
   code:blank
   (code:contract new-fish : num -> fish-object)
   (define (new-fish init-weight)
     (local [(define WEIGHT init-weight)
             (define (feed n)
               (begin
                 (set! WEIGHT (+ WEIGHT n))
                 WEIGHT))]
       feed)))
  
  (define-code/string/scale 
   1.0
   (fish-obj-example-run fish-obj-example-code fish-obj-example-string)
   (define alice (new-fish 7))
   (define bob (new-fish 6))
   (define norman (new-fish 12))
   code:blank
   (define my-favorite-fish (list alice norman))
   (define all-my-fish (list alice bob norman))
   code:blank
   (alice 4) "should be" 11
   code:blank
   ((first my-favorite-fish) 0) "should be" 11
   ((first all-my-fish) 0) "should be" 11)
  
  (slide/title
   "Encapsulation"
   (page-para "Two lectures ago, we encapsulated a fish with a GUI:")
   (code 
    (code:comment "A live-fish is")
    (code:comment "  (num -> num)")
    code:blank
    (code:contract make-fish : num -> live-fish)
    (define (make-fish init-weight)
      (local [(define WEIGHT init-weight)
              (define (feed n) ...)
              ...]
        (begin
          (create-window ...)
          feed))))
   'next
   (page-para "By returning" (code feed) ", we enable"
              "programs that process groups of fish"))

  (slide/title
   "Objects"
   (page-para "Maybe we don't need the GUI, but we'd like to represent fish identities")
   'alts
   (list (list
          fish-obj-example-code
          (mk-copy fish-obj-example-string))
         (list
          fish-object-code
          (mk-copy fish-object-string))))
  
  (slide/title
   "Armadillos"
   (page-para "How about armadillos?")
   (code (code:contract new-dillo : num bool -> (num -> num))
         (define (new-dillo init-weight init-alive?)
           (local [(define WEIGHT init-weight)
                   (define ALIVE? init-alive?)
                   (define (feed n)
                     (begin
                       (set! WEIGHT (+ WEIGHT n))
                       WEIGHT))]
             feed)))
   'next
   (blank)
   (page-para "We can feed a dillo this way, but we can't check whether it's alive..."))

  (slide/title
   "Armadillos"
   (code (define (new-dillo init-weight init-alive?)
           (local [(define WEIGHT init-weight)
                   (define ALIVE? init-alive?)
                   (define (feed n)
                     (begin
                       (set! WEIGHT (+ WEIGHT n))
                       WEIGHT))
                   (define (is-alive?)
                     ALIVE?)
                   (define (set-alive a?)
                     (set! ALIVE? a?))]
             ... feed ... is-alive?
             ... set-alive ...)))
   'next
   (blank)
   (page-para "How can we return three functions?"))
   
  (define-code/string/scale 
   0.8
   (dillo-object-run dillo-object-code dillo-object-string)
   (code:comment "A dillo-object is")
   (code:comment "  (make-dillo (num -> num) (-> bool) (bool -> void))")
   (define-struct dillo (feed is-alive? set-alive))
   code:blank
   (code:contract new-dillo : num bool -> dillo-object)
   (define (new-dillo init-weight init-alive?)
     (local [(define WEIGHT init-weight)
             (define ALIVE? init-alive?)
             (define (feed n)
               (begin
                 (set! WEIGHT (+ WEIGHT n))
                 WEIGHT))
             (define (is-alive?)
               ALIVE?)
             (define (set-alive a?)
               (set! ALIVE? a?))]
       (make-dillo feed is-alive? set-alive))))
   
  (slide/title
   "Armadillo Objects"
   dillo-object-code
   (mk-copy dillo-object-string))

  (define-code/string/scale
   0.9
   (dillo-obj-example-run dillo-obj-example-code dillo-obj-example-string)
   (define cindy (new-dillo 5 true))
   (define dan (new-dillo 8 true))
   code:blank
   ((dillo-feed cindy) 2) "should be" 7
   ((dillo-feed dan) 1) "should be" 9
   ((dillo-feed cindy) 0) "should be" 7
   code:blank
   (code:contract run-over! : dillo -> void)
   (define (run-over! d)
     ((dillo-set-alive d) false))
   code:blank
   ((dillo-alive? dan)) "should be" true
   (run-over! dan) "should be" (void)
   ((dillo-alive? dan)) "should be" false
   ((dillo-alive? cindy)) "should be" true)

  (slide/title
   "Armadillo Object Examples"
   dillo-obj-example-code
   (mk-copy dillo-obj-example-string))
  
  (slide/title
   "Disallowing Armadillo Resurrection"
   (scale/improve-new-text
    (code
     (code:comment "A dillo-object is")
     (code:comment "  (make-dillo (num -> num) (-> bool) (-> void))")
     (define-struct dillo (feed is-alive? run-over!))
     code:blank
     (code:contract new-dillo : num bool -> dillo-object)
     (define (new-dillo init-weight init-alive?)
       (local [(define WEIGHT init-weight)
               (define ALIVE? init-alive?)
               (define (feed n)
                 (begin
                   (set! WEIGHT (+ WEIGHT n))
                   WEIGHT))
               (define (is-alive?)
                 ALIVE?)
               (define (run-over!)
                 (set! ALIVE? false))]
         (make-dillo feed is-alive? run-over!))))
    0.8))
  
  (define THING-object (hbl-append (code _THING) (code -object)))
  (define new-THING (hbl-append (code new-) (code _THING)))
  (define make-THING (hbl-append (code make-) (code _THING)))
  
  (slide/title
   "General Pattern for Encapsulating Objects"
   (code
    (code:comment "A " #,THING-object " is")
    (code:comment " (" #,make-THING " " #,(code _method-type) " ...)")
    (define-struct _THING (_METHOD ...))
    code:blank
    (code:contract #,new-THING : _init-type ... -> #,THING-object)
    (define (#,new-THING _init-val ...)
      (local [(define _STATE _init-val)
              ...
              (code:contract _METHOD : _method-type)
              (define (_METHOD _arg ...)
                ...)]
        (#,make-THING _METHOD ...))))
   'next
   (blank)
   (colorize
    (page-para* (bt "Note:") "implementation depends on the"
                "operations (a.k.a. methods) that you want")
    BlueColor))

  (slide/title/center
   "Encapsulation"
   (page-para "Encapsulation")
   (page-item "Groups related functions with data")
   (page-item "Controls access/modification of state")
   'next
   (blank)
   (colorize (hline (* 3/4 client-w) 1) GreenColor)
   (blank)
   (page-para "Encapsulation is a key idea behind languages like Java")
   'next
   (blank)
   (page-para "Still, one other idea is more important:")
   (cc-superimpose
    (page-subitem "Data-driven design")
    (colorize (page-para/r "This is what you know") BlueColor))
   (page-para "One more idea is equally important:")
   (cc-superimpose
    (page-subitem "Extensible data definitions")
    (colorize (page-para/r "We'll see this soon") BlueColor)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (outline 'vec)

  (slide/title
   "The Truth about Vectors"
   (page-para "A vector is an object with state")
   (blank)
   (code (define v (vector 'a 'b 'c))
         code:blank
         (vector-ref v 0) "should be" 'a
         code:blank
         (vector-set! v 0 'd)
         (vector-ref v 0) "should be" 'd))
  
  (slide/title
   "A Zoo with Cage"
   (page-para "Let's keep our armadillos in cages")
   (page-item "Each cage holds one armadillo")
   (page-item "If we have 5 cages, we can represent the set of cages with a vector of size 5")
   (blank)
   (code (define cages 
           (vector false false false false false))
         
         (define cindy (new-dillo 5 true))
         (define dan (new-dillo 8 true))

         (vector-set! cages 0 cindy)
         (vector-set! cages 1 dan)))
  
  (slide/title
   "Moving Armadillos"
   (problem "Implement" (code move-dillo) "which takes a" (code dillo-object)
            "and a cage number, and move the dillo to the cage number")
   'next
   (blank)
   (code (code:contract move-dillo : dillo-object n -> void)
         code:blank
         (code:comment "continuing from the previous example")
         (move-dillo cindy 3) "should be" (void)
         (vector-ref cages 3) "should be" cindy))
  
  (slide/title
   "Moving Armadillos"
   (page-para "First attempt:")
   (code (define (move-dillo d n)
           (vector-set! cages n d)))
   'next
   (blank)
   (blank)
   (colorize (page-para "Problem: the dillo is still in its old cage") RedColor)
   (blank)
   (code (move-dillo cindy 3) "should be" (void)
         (vector-ref cages 3) "should be" cindy
         code:blank
         (vector-ref cages 0) "should be" false
         (code:comment "but currently we get cindy")))
  
  (with-steps
   (move use-find find-at)
   (slide/title/tall
    "Finding and Moving Armadillos"
    (vl-append
     line-sep
     (code (define (move-dillo d n)
             (begin
               (vector-set! cages 
                            #,(cbl-superimpose
                               ((vbefore use-find) (code ...))
                               ((vafter use-find)
                                (code (find-dillo d))))
                            #,(code false))
               (vector-set! cages n d)))
           code:blank)
     ((vafter use-find)
      (code (code:contract find-dillo : dillo -> num)))
     (lt-superimpose
      ((vbetween-excl use-find find-at)
       (code (define (find-dillo d)
               ...)))
      ((vafter find-at)
       (code (define (find-dillo d)
               (find-dillo-at d 0)))))
    (code code:blank)
    ((vafter find-at)
     (vl-append
      line-sep
      (code (code:contract find-dillo-at : dillo num -> num))
      (code (define (find-dillo-at d n)
              (cond
                [(same? d (vector-ref cages n)) n]
                [else (find-dillo-at d (add1 n))]))))))))
  
  (slide/title
   "Comparing Armadillos"
   (code (code:contract same? : dillo-object dillo-object -> bool)
         (define (same? d1 d2)
           (and (= ((dillo-feed d1) 0)
                   ((dillo-feed d2) 0))
                (boolean=? ((dillo-alive? d1))
                           ((dillo-alive? d2))))))
   (blank)
   (code (define eddie (new-dillo 7 true))
         (define fran (new-dillo 7 true))
         code:blank
         (same? eddie fran) "should be" true)
   'next
   (colorize
    (page-para* "But that's not right" sym:emdash (code eddie) "and" (code fran)
                "are different armadillos")
    RedColor))
  
  (slide/title
   "Detecting an Armadillo"
   (page-para "If" (code d1) "and" (code d2) "are the same, then feeding" (code d1)
              "should grow" (code d2) ":")
   (scale/improve-new-text
    (code
     (define (same? d1 d2)
       (local [(define orig-d2-size ((dillo-feed d2) 0))]
         (begin
           ((dillo-feed d1) 1)
           (local [(define later-d2-size ((dillo-feed d2) 0))]
             (begin
               ((dillo-feed d1) -1)
               (> later-d2-size orig-d2-size)))))))
    0.8)
   'next
   (blank)
   (page-para "Granted, this is a harsh way to compare armadillos, so"
              (bt "Advanced") "provides" (code eq?))
   (code
    (define (same? d1 d2)
      (eq? d1 d2))))
   
  )
