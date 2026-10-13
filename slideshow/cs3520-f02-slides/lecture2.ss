
(module lecture2 (lib "slideshow.ss" "texpict")

  (require "utils/colors.ss"
	   "utils/utils.ss"
	   "utils/alg.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Writing Functions in Scheme"
   (page-item "Suppose we want a function" (alg-code "ct")
	      "which takes a list of symbols and returns the"
	      "number of symbols in the list")
   (blank)
   (alg-code "(ct '(a b c)) ->> 3")
   (alg-code "(ct '()) ->> 0")
   (alg-code "(ct '(x y z w t)) ->> 5")
   (blank)
   (page-para* "How can we write this function?"))

  (define (count-body l did-null extra-paren)
    (alg-code*
     "  (cond"
     (format "    [~a 0]" (or did-null (format "(null? ~a)" l)))
     (format "    [else (+ 1 (ct (cdr ~a)))])~a" l extra-paren)))

  (define count-prog
    (vl-append
     line-sep
     (alg-code "(define (ct l)")
     (count-body "l" #f ")")
     (alg-code " ")))

  (define count-examples
    (alg-code*
     ";;  (ct '()) ->> 0"
     ";;  (ct '(a b c)) ->> 3"))

  (slide/title
   "Writing Functions in Scheme"
   (page-item "Answer #1: Have the instructor write it")
   (blank)
   (vl-append
    line-sep
    (alg-code ";; ct : list-of-sym --> num")
    count-examples
    count-prog))

  (slide/title
   "Checking My Answer: Empty List"
   'alts
   (steps count-prog
	  (alg-code "(ct '())")
	  (count-body "'()" #f "")
	  (count-body "'()" "#t" "")
	  (alg-code "0")))

  (define (add-one p)
    (vl-append
     line-sep
     (alg-code "(+ 1")
     (htl-append
      (alg-code "   ")
      p)))

  (slide/title
   "Checking My Answer: List of 3 Symbols"
   'alts
   (map 
    (lambda (l)
      (map (lambda (p)
	     (scale p 0.8))
	   l))
    (steps count-prog
	   (alg-code "(ct '(a b c))")
	   (count-body "'(a b c)" #f "")
	   (count-body "'(a b c)" "#f" "")
	   (alg-code "(+ 1 (ct (cdr '(a b c))))")
	   (add-one (alg-code "(ct '(b c)))"))
	   (add-one (count-body "'(b c)" #f ")"))
	   (add-one (count-body "'(b c)" "#f" ")"))
	   (add-one (add-one (alg-code "(ct (cdr '(b c)))))")))
	   (add-one (add-one (alg-code "(ct '(c))))")))
	   (add-one (add-one (count-body "'(c)" #f "))")))
	   (add-one (add-one (count-body "'(c)" "#f" "))")))
	   (add-one (add-one (add-one (alg-code "(ct (cdr '(c))))))"))))
	   (add-one (add-one (add-one (alg-code "(ct '()))))"))))
	   (add-one (add-one (add-one (count-body "'()" #f ")))"))))
	   (add-one (add-one (add-one (count-body "'()" "#t" ")))"))))
	   (add-one (add-one (add-one (alg-code "0)))"))))
	   (add-one (add-one (alg-code "1))")))
	   (add-one (alg-code "2)"))
	   (alg-code "3"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (recipe hilite? general?)
    (list
     (page-item (format "Locate or write ~adata definition~a"
			(if general? "" "a ")
			(if general? "s" "")))
     (page-item (format "Write ~acontract~a"
			(if general? "" "a ")
			(if general? "s" "")))
     (page-item "Write examples")
     (let ([i (page-item (format "Create a template that follows the shape of the data definition~a"
				 (if general?
				     ", one for each data definition"
				     "")))])
       (if hilite?
	   (background YellowColor i)
	   i))
     (page-item (format "Convert the template~a to the final function~a"
			(if general? "s" "")
			(if general? "s" "")))
     (page-item "Run examples as tests")))

  (slide/title
   "Writing Functions in Scheme: Answer #2"
   (page-para "Answer #2: Use the general design recipe")
   'next
   'alts
   (list (recipe #f #f)
	 (recipe #t #f))
   'next
   (blank)
   (page-para* (colorize (t "works 90% of the time") BlueColor)))

  (define los-datadef
    (grammar-table 
     (list (alg-code "list-of-sym") eqls (alg-code "'()") (blank)
	   (blank)                  eqls (alg-code "(cons symbol list-of-sym)") (blank))))

  (slide/title 
   "Data Definitions"
   (page-para "What is a \"list of symbols\"?")
   'next
   (blank)
   los-datadef
   'next
   (blank)
   (page-item "Sometimes the" (dt "data definition") "is given,"
	      "somtimes you have to create it")
   (page-item "Usually include it in your code as a comment"))
  
  (slide/title 
   "Contracts"
   (page-para "A" (dt "contract") "is a comment that identifies"
	      "set of input values and output values")
   (blank)
   (alg-code ";; ct: list-of-sym --> num")
   'next
   (blank)
   (page-item "All mentioned data sets should have a data definition somewhere"))

  (slide/title 
   "Examples"
   (page-para "Examples (usually in comments at first) help clarify the purpose of the function")
   (blank)
   count-examples
   'next
   (blank)
   (page-item "Make sure that every case in the data definition is covered at least once"))

  (define (ct-prog hilite)
    (prog hilite
          "(define (ct l)
  (cond
   [(null? l) ...]
   [(pair? l) ...(car l)...(ct (cdr l))...]))"))


  (slide/title
   "Template"
   (page-para "A" (dt "template") "reflects the structure of the input"
	      "according to the data definition")
   los-datadef
   'alts
   (list (list (ct-prog "none"))
         (list (ct-prog "[(]cond|([[].*[]][)]?)")
	       (page-item "Two cases in data definition implies" (alg-code "cond")
			  "with two cond-lines"))
	 (list (ct-prog ".((pair[?])|(null[?])) l.")
	       (page-item "Corresponding predicate for each data case"))
	 (list (ct-prog ".((car)|(cdr)) l.")
	       (page-item "Extract parts in cases with meta-variables"))
	 (list (ct-prog ".ct .cdr l..")
	       (page-item "Recursive call for self-references in data definition"))
	 (list (ct-prog "none")
	       (background YellowColor
			   (page-item "A template depends only on the input data;"
				      "it ignores the function's purpose"))
	       'next
	       (page-para* "(Nevertheless, generating a template, which is fairly automatic,"
			   "usually provides most of the function)"))))
  
  (slide/title
   "Template to Function"
   (page-para "Transform template to function line-by-line")
   'alts
   (list (list (ct-prog "none"))
	 (list (prog "0"
	 "(define (ct l)
  (cond
   [(null? l) 0]
   [(pair? l) ...(car l)...(ct (cdr l))...]))"))
	 (list (prog "(.[+] 1)|( [)])"
	 "(define (ct l)
  (cond
   [(null? l) 0]
   [(pair? l) (+ 1 (ct (cdr l)) )]))")
	       (blank)
	       (page-item "Sometimes, a part of the template isn't needed"))))

  (slide/title
   "Reminder: Recipe"
   (blank)
   'alts
   (list (recipe #t #f)))

  (slide/title
   "Reminder: Template Steps"
   (page-item "Create a" (alg-code "cond") "expression with one line for each case in the data definition")
   (page-item "Write down a predicate for each case")
   (page-item "For the answer, extract parts in cases with meta-variables")
   (page-item "For each self-reference in the data definition, add a recursive call")
   (background YellowColor (page-para* "Shape of template shape == Shape of data definition")))

  (slide/title/center
   "More Examples"
   (page-para* "(more examples in class)"))

  (slide/title
   "Generalized Recipe"
   (blank)
   'alts
   (list (recipe #f #t)))

  'done)
