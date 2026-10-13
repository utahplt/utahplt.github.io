#cs
(module lecture10 (lib "slideshow.ss" "slideshow")  
  (require "utils/colors.ss"
           "utils/utils.ss"
           "utils/code.ss"
           "utils/lec0.ss"
           "utils/pipe.ss"
	   (lib "step.ss" "slideshow")
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

  (define (black-split-arrows top bottom)
    (vc-append
     (* 2 gap-size)
     (top (arrow (* 3/2 gap-size) (* pi 1/4)))
     (bottom (arrow (* 3/2 gap-size) (* pi -1/4)))))

  (define split-arrows
    (colorize
     (black-split-arrows values values)
     GreenColor))
  (define bottom-split-arrows
    (colorize
     (black-split-arrows ghost values)
     GreenColor))

  (define no-more-split-arrows
    (colorize
     (black-split-arrows values values)
     RedColor))

  (define top-no-more-split-arrows
    (colorize
     (black-split-arrows values ghost)
     RedColor))

  (define (name i s)
    (let ([t (t s)])
      (vc-append
       line-sep
       (ghost t)
       i
       t)))

  (define mtree (name mike "Mike"))

  (define (no-more-gossip p when)
    (hc-append
     gap-size
     p
     (when no-more-split-arrows)))
    

  (define (dead-fish p after)
    (let ([w (pict-width p)]
	  [h (pict-height p)])
      (lt-superimpose
       (inset p (/ w 3) (/ w 4) 0 (/ w 4))
       (after
	(vr-append
	 (inset (cc-superimpose
		 (cloud (* 6/5 w) (* 3/4 w))
		 (standard-fish (* 2/3 w) (* 1/3 w) 'left "red" 'x))
		0 0 0 (- (* 1/8 w)))
	 (colorize (filled-ellipse (* 1/6 w) (* 1/6 w)) "gray")
	 (colorize (filled-ellipse (* 1/8 w) (* 1/8 w)) "gray"))))))

  (define (joseph-mill vafter-no-more)
    (let ([down (/ (pict-height joseph) 2)])
      (inset
       (no-more-gossip (name joseph "Joseph") vafter-no-more)
       0 down 0 0)))

  (define (amir-mill vafter-fourth vafter-no-more)
    (hc-append
     gap-size
     (name amir "Amir")
     (vafter-fourth
      (cc-superimpose
       bottom-split-arrows
       (vafter-no-more top-no-more-split-arrows)))
     (vafter-fourth
      (joseph-mill vafter-no-more))))

  (define (rumor-mill vafter-root vafter-rumor vafter-second vafter-third vafter-fourth vafter-no-more)
    (inset
     (hc-append
      gap-size
      (vafter-root (dead-fish (name seiichi "Seiichi") vafter-rumor))
      (vafter-second split-arrows)
      (vafter-second
       (inset
	(lt-superimpose
	 (no-more-gossip mtree vafter-no-more)
	 (inset (hc-append
		 gap-size
		 (name lindsey "Lindsey")
		 (vafter-third split-arrows)
		 (vafter-third 
		  (vl-append
		   gap-size
		   (amir-mill vafter-fourth vafter-no-more)
		   (no-more-gossip (name derrick "Derrick") vafter-no-more))))
		0 (/ (pict-height mtree) 3) 0 0))
	0 (/ (pict-height mtree) 3) 0 0)))
     0 (- (/ (pict-height mtree) 3)) 0 0))

  (with-steps
   (setup~ root~ rumor~ second~ third~ fourth simplify~ no-more)
   (slide/title
    "Tracking Rumors"
    (cc-superimpose
     ((vbetween setup~ fourth)
      (page-para "Suppose that we want to track gossip in a rumor mill"))
     ((vafter simplify~)
      (colorize
       (page-para "Simplifying assumption: each person tells at most two others")
      BlueColor)))
    (rumor-mill (vafter root~) (vafter rumor~) (vafter second~) (vafter third~) (vafter fourth) (vafter no-more))))

  (define example-mill (rumor-mill values ghost values values values values))

  (define rumor-mill-defn
    (scale/improve-new-text
     (code (code:comment "A rumor-mill is either")
	   (code:comment "  - empty")
	   (code:comment "  - (make-gossip image rumor-mill rumor-mill)"))
     0.9 0.9))

  (slide/title
   "Representing Rumor Mills"
   (scale example-mill 0.5 0.5)
   (blank)
   'alts
   (list (list
	  (page-para "Is a rumor mill simply a list of people?")
	  'next
	  (page-para "No, because there are relationships among people"))
	 (list
	  (page-para "How about this?:")
	  (code (code:comment "A person is")
		(code:comment "  (make-person image person person)"))
	  'next
	  (page-para "No, because some people don't gossip to anyone else"
		     (symbol 190) "or they gossip to an empty rumor mill..."))
	 (list
	  (page-para "How about this?:")
	  (vl-append
	   line-sep
	   rumor-mill-defn
	   (code (define-struct gossip (who next1 next2))))
	  'next
	  (page-para "This looks promising..."))))
	  
   
  (define full-rumor-mill-val
    (code 
     (make-gossip #,(code-align small-seiichi) 
		  (make-gossip #,(code-align small-mike) empty empty)
		  (make-gossip #,(code-align small-lindsey)
			       (make-gossip #,(code-align small-amir)
					    empty 
					    (make-gossip #,(code-align small-joseph) empty empty))
			       (make-gossip #,(code-align small-derrick) empty empty)))))

  (slide/title
   "Example Rumor Mills"
   rumor-mill-defn
   (blank)
   'alts
   (list (list (code empty))
	 (list (code (make-gossip #,(code-align small-joseph) empty empty))
	       (joseph-mill values))
	 (list (code (make-gossip #,(code-align small-amir)
				  empty
				  (make-gossip #,(code-align small-joseph) 
					       empty empty)))
				  
	       (scale (amir-mill values values) 0.8 0.8))
	 (list (scale full-rumor-mill-val 0.5 0.5)
	       (scale example-mill 0.4 0.4))))

  (slide/title/tall
   "Example Using Abbreviations"
   (scale/improve-new-text
    (vl-append
     gap-size
     (code (define joseph-mill 
	     (make-gossip #,(code-align small-joseph) empty empty)))
     (code (define amir-mill 
	     (make-gossip #,(code-align small-amir) empty joseph-mill)))
     (code (define derrick-mill 
	     (make-gossip #,(code-align small-derrick) empty empty)))
     (code (define lindsey-mill 
	     (make-gossip #,(code-align small-lindsey) amir-mill derrick-mill)))
     (code (define mike-mill 
	     (make-gossip #,(code-align small-mike) empty empty)))
     (code (define seiichi-mill 
	     (make-gossip #,(code-align small-seiichi) mike-mill lindsey-mill))))
    0.8 0.8))

  (define rumor-mill-tmpl
    (scale/improve-new-text
     (code (define (func-for-rumor-mill rm)
	     (cond
	      [(empty? rm) ...]
	      [(gossip? rm)
	       ... (gossip-who rm)
	       ... (func-for-rumor-mill (gossip-next1 rm))
	       ... (func-for-rumor-mill (gossip-next2 rm)) ...])))
     0.8 0.8))

  (slide/title
   "Programming with Rumors"
   'alts~
   (list (list rumor-mill-defn)
	 (list (add-gp-arrow
		(add-gp-arrow
		 rumor-mill-defn
		 2/3 23/30 1/3 8/30)
		4/5 23/30 1/3 8/30)))
   'next
   (blank)
   'alts~
   (list (list rumor-mill-tmpl)
	 (list (add-gp-arrow
		(add-gp-arrow
		 rumor-mill-tmpl
		 1/2 52/70 1/2 8/70)
		1/2 62/70 1/2 8/70))))

  (slide/title
   "Rumor Program Examples"   
   (problem "Implement the function" (code informed?) "which takes a person image and a rumor mill"
	    "and determines whether the person is part of the rumor mill")
   (problem "Implement" (code rumor-delay) "which takes a rumor mill and"
	    "determines the maximum number of days required for a rumor to reach everyone,"
	    "assuming that each person waits a day before passing on a rumor")
   (problem "Implement" (code add-gossip) "which takes a rumor mill and two person images"
	    (symbol 190) "one new and one old" (symbol 190)
	    "and adds the new person to the rumor mill, receiving rumors from the old person;"
	    "the old person must not already have two next persons")
   (problem "Implement" (code rumor-chain) "which takes a person image and a rumor mill and"
	    "returns a list of person images representing everyone who must pass on the"
	    "rumor for it to reach the given person; return" (code false) "if the given person"
	    "is never informed"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define pipeline-defn
    (code (code:comment "A pipeline is either")
          (code:comment " - bool")
          (code:comment " - (make-straight sym pipeline)")
          (code:comment " - (make-branch pipeline pipeline)")))
  
  (slide/title
   "More Pipes"
   (page-item "In the Mid-Term I example, we had all straight pipes in a pipeline")
   (page-item "Real pipes end in faucets (open or closed) and sometimes branch")
   'alts
   (list (list (plain-faucet))
         (list (vl-append (plain-faucet) (water)))
         (list (vr-append (ht-append (plain-straight) (plain-straight) (plain-faucet))
                          (water)))
         (list (ht-append (copper-straight) (lead-straight) (plain-faucet)))
         (list (ht-append (plain-straight) (plain-fork)
                          (vl-append (ht-append (plain-faucet))
                                     (ht-append (lead-straight) (plain-faucet))))))
   'next
   (vl-append
    line-sep
    pipeline-defn
    (code (define-struct straight (kind next))
          (define-struct branch (next1 next2)))))
  
  (define (ex code i)
    (list code
          (blank)
          i))

  (define big-pipeline
    (scale
     (ht-append (plain-fork 1)
		(vl-append
		 (ht-append (plain-fork)
			    (vl-append (ht-append (copper-straight) (plain-faucet))
				       (ht-append (plain-faucet) (water))))
		 (ht-append (plain-fork)
			    (vl-append (plain-faucet)
				       (plain-faucet)))))
     0.8 0.8))
  
  (slide/title
   "Example Pipelines"
   pipeline-defn
   (blank)
   'alts
   (list (ex (code false)
             (plain-faucet))
         (ex (code true)
             (vl-append (plain-faucet) (water)))
         (ex (code (make-straight 'copper false))
             (ht-append (copper-straight) (plain-faucet)))
         (ex (code (make-straight 'copper 
                                  (make-straight 'lead false)))
             (ht-append (copper-straight) (lead-straight) (plain-faucet)))
         (list
	  (rb-superimpose
	   (inset
	    (code (make-branch
		   (make-branch (make-straight 'copper true)
				false)
		   (make-branch false
				false)))
	    0 0 0 (* (pict-height big-pipeline) 3/4))
	    big-pipeline))))
                   
  (define pipeline-tmpl
    (scale/improve-new-text
     (code (define (func-for-pipeline pl)
	     (cond
	      [(boolean? pl) ...]
	      [(straight? pl)
	       ... (straight-kind pl)
	       ... (func-for-pipeline (straight-next pl)) ...]
	      [(branch? pl)
	       ... (func-for-pipeline (branch-next1 pl))
	       ... (func-for-pipeline (branch-next2 pl)) ...])))
     0.8 0.8))
     
  (slide/title
   "Programming with Pipelines"
   'alts~
   (list (list pipeline-defn)
	 (list (add-gp-arrow
		(add-gp-arrow
		 (add-gp-arrow
		  pipeline-defn
		  4/5 22/40 1/3 8/40)
		 2/3 32/40 1/3 8/40)
		4/5 32/40 1/3 8/40)))
   'next
   (blank)
   'alts~
   (list (list pipeline-tmpl)
	 (list (add-gp-arrow
		(add-gp-arrow
		 (add-gp-arrow
		  pipeline-tmpl
		  1/3 54/90 1/3 8/90)
		 1/3 74/90 1/3 8/90)
		1/3 84/90 1/3 8/90))))

  (slide/title
   "Pipeline Examples"   
   (problem "Implement the function" (code water-running?) "which takes a pipeline"
	    "and determines whether any faucets are open")
   (problem "Implement the function" (code modernize) "which takes a pipeline"
	    "and converts all" (code 'lead) "straight pipes to"
	    (code 'copper))
   (problem "Implement the function" (code off) "which takes a pipeline"
	    "and turns off all the faucets")
   (problem "Implement the function" (code lead-off) "which takes a pipeline"
	    "and turns off all the faucets that receive water through a lead pipe")
   (problem "Implement the function" (code twice-as-long) "which takes a pipeline"
	    "and inserts a" (code 'copper) "straight pipe before every existing"
	    "piece of the pipeline"))

  
  )
