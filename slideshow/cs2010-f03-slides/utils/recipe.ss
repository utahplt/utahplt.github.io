#cs
(module recipe (lib "slideshow.ss" "slideshow")

  (require "colors.ss"
           "utils.ss"
           "code.ss"
	   (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))

  (provide design-recipe-I
	   design-recipe-II
	   design-recipe-III
	   design-recipe-X)

  (define (vc-append/skip-nothing . l)
    (apply vc-append (filter (lambda (x) (not (eq? x 'nothing))) l)))

  (define (design-recipe-X title level hilite)
    (slide/title/tall
     (or title
	 (format "Design Recipe ~a" (case level
				      [(1) "I"]
				      [(2) "II"]
				      [(3) "III"])))
     (vc-append/skip-nothing
      (if (level . > . 1)
	  (* gap-size 2/3)
	  gap-size)
      ;;
      (recipe-item "Data" (eq? hilite 'data))
      (if (level . > . 1)
	  (recipe-desc "Understand the input data")
	  (recipe-desc "Understand the input data:"
		       (code num) "," (code bool) "," (code sym)
		       ", or" (code image)))
      ;;
      (recipe-item "Contract, Purpose, and Header" (eq? hilite 'cph))
      (recipe-desc "Describe (but don't write) the function")
      ;;
      (recipe-item "Examples" (eq? hilite 'examples))
      (recipe-desc "Show what will happen when the function is done")
      ;;
      (if (level . > . 1)
	  (recipe-item "Template" (eq? hilite 'template))
	  'nothing)
      (if (level . > . 1)
	  (recipe-desc "Set up the body based on the input data (and" (it "only") "the input)")
	  'nothing)
      ;;
      (recipe-item "Body" (eq? hilite 'body))
      (recipe-desc "The most creative step: implement the function body")
      ;;
      (recipe-item "Test" (eq? hilite 'test))
      (recipe-desc "Run the examples"))))

  (define (design-recipe-I hilite)
    (design-recipe-X #f 1 hilite))

  (define (design-recipe-II hilite)
    (design-recipe-X #f 2 hilite))

  (define (design-recipe-III hilite)
    (design-recipe-X #f 3 hilite)))
