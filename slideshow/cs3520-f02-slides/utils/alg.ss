
(module alg (lib "slideshow.ss" "texpict")
  
  (require "colors.ss" "utils.ss")

  (define MetaColor BlueColor)
  (define TypeColor GreenColor)
  (define @HiliteColor RedColor)

  (define alg-patterns
    (list
     (regexp "^\\^")
     (lambda (s) (t " "))

     (regexp "^[*][*],")
     (lambda (s)  (colorize (hbl-append (t "*")
					(text "(,)" `(superscript . ,main-font) font-size))
			    RedColor))
     
     (regexp "^[*][*]")
     (lambda (s) (colorize (t "*") RedColor))

     (regexp "^({{|}})")
     (lambda (s) (colorize (t (string (string-ref s 0))) RedColor))

     (regexp "^[+][+]")
     (lambda (s) (colorize (text "+" `(superscript . ,main-font) font-size) RedColor))

     (regexp "^[|][-]")
     (lambda (s) (colorize turnstyle MetaColor))
     
     (regexp "^[|]")
     (lambda (s) (colorize (tt " | ") MetaColor))

     (regexp "^Omega")
     (lambda (s) (text "W" 'symbol font-size))

     (regexp "^(numTd|boolTd|intTd|voidTd|colorfishTd|fishTd)")
     (lambda (s) (bt (substring s 0 (- (string-length s) 2))))

     (regexp "^(numT|boolT|intT|fishT|objectT|colorfishT|c1T|voidT|dogT|animalT|hairstyleT|treeT)")
     (lambda (s) (colorize (tt (substring s 0 (- (string-length s) 1))) TypeColor))

     (regexp "^(number|primapp-|a-program|a-funcdef|-prim|-env|identity|prime[?])")
     (lambda (s) (bt s))

     (regexp "^#[a-zA-Z-]+#")
     (lambda (s) (bt (substring s 1 (- (string-length s) 1))))

     (regexp "^(prog|id|expr|cexpr|defn|funcdef|num|bool|symbol|cond-line|val|expval|denval|lst|defined-id|list-of-num|l-o-n|list-of-sym|prim|env|reference|type|tyexpr|class-decl|method-decl|field-decl|[?][?][?][?])")
     (lambda (s) (colorize (t (format "<~a>" s)) MetaColor))

     (regexp "^Env")
     (lambda (s) (colorize (it "E") MetaColor))
     (regexp "^Type")
     (lambda (s) (colorize (it "T") MetaColor))
     (regexp "^Expr")
     (lambda (s) (colorize (it "e") MetaColor))

     (regexp "^Lambda")
     (lambda (s) (symbol (char->integer #\l)))

     (regexp "^Proc")
     (lambda (s) (colorize (t "<proc>") MetaColor))

     (regexp "^(define|and|or|#f|#t|'|cond|else|cons)")
     (lambda (s) (bt s))

     (regexp "^isin[?]")
     (lambda (s) (colorize (hbl-append sym:in (bt "?")) RedColor))

     (regexp "^isin")
     (lambda (s) (colorize sym:in MetaColor))

     (regexp "^[.][.][.][.]")
     (lambda (s) (colorize (t "...") MetaColor))
     
     (regexp "^[a-zA-Z.][a-zA-Z0-9.?!]*")
     (lambda (s) (bt s))
     
     (regexp "^~[a-zA-Z.][a-zA-Z0-9.?!]*")
     (lambda (s) (it (substring s 1 (string-length s))))
     
     (regexp "^,,")
     (lambda (s) (colorize (t ",") MetaColor))
     
     (regexp "^==")
     (lambda (s) (colorize (t "=") MetaColor))
     
     (regexp "^[(][*]")
     (lambda (s) (colorize (t "(") TypeColor))
     
     (regexp "^[*][)]")
     (lambda (s) (colorize (t ")") TypeColor))
     
     (regexp "^[(][[][*]")
     (lambda (s) (colorize (t "[") TypeColor))
     
     (regexp "^[*][]][)]")
     (lambda (s) (colorize (t "]") TypeColor))
     
     (regexp "^[(]~")
     (lambda (s) (t "("))
     
     (regexp "^~[)]")
     (lambda (s) (t ")"))
     
     (regexp "^[*][-]>")
     (lambda (s) (colorize sym:rightarrow TypeColor))
     
     (regexp "^[*]X[*]")
     (lambda (s) (colorize (symbol 180) TypeColor))

     (regexp "^[*]:[*]")
     (lambda (s) (colorize (tt ":") TypeColor))

     (regexp "^[-]>>")
     (lambda (s) (colorize (hbl-append sym:rightarrow sym:rightarrow) MetaColor))
     
     (regexp "^[-][-]>")
     (lambda (s) (colorize (tt " -> ") MetaColor))
     
     (regexp "^[-]>")
     (lambda (s) (colorize sym:rightarrow MetaColor))
     
     (regexp "^_>")
     (lambda (s) (tt "->"))
     
     (regexp "^__")
     (lambda (s) (tt "_"))
     
     (regexp "^<loopforever>")
     (lambda (s) (colorize (it "[loop forever]") MetaColor))
     
     (regexp "^[0-9]+")
     (lambda (s) (t s))
     
     (regexp "^_[0-9]+")
     (lambda (s) (colorize (text (substring s 1 (string-length s)) `(subscript bold . ,main-font) (inexact->exact (floor (* 1.2 font-size))))
			   MetaColor))

     (regexp "^_[a-zA-Z]")
     (lambda (s) (colorize (text (substring s 1 (string-length s)) `(subscript bold italic . ,main-font) (inexact->exact (floor (* 1.2 font-size))))
			   MetaColor))

     (regexp "^@[a-zA-Z<>0-9]+")
     (lambda (s) (colorize (bt (substring s 1 (string-length s))) @HiliteColor))
     
     (regexp "^([()*,+/ =_:{}<>;#&?%]|[-]|[[]|[]])")
     (lambda (s) (text s main-font font-size))

     ;; non-breaking space:
     (regexp (format "^~a" (integer->char 160)))
     (lambda (s) (text s main-font font-size))))

  (define (alg-code$ p s)
    (if (string=? s "")
	(t "")
	(let ([m (and p (regexp-match-positions p s))])
	  (if m
	      (hbl-append
	       (alg-code$ #f (substring s 0 (caar m)))
	       (colorize (alg-code$ #f (substring s (caar m) (cdar m)))
			 PurpleColor)
	       (alg-code$ p (substring s (cdar m) (string-length s))))
	      (code/patterns alg-patterns s)))))

  (define (alg-code t)
    (alg-code$ #f t))

  (define (alg-code$* p . l)
    (apply
     vl-append
     line-sep
     (map (lambda (x) (if (string? x) (alg-code$ p x) x)) l)))

  (define (alg-code* . l)
    (apply alg-code$* #f l))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define eqls (colorize (t "::=") MetaColor))
  (define -or- eqls)

  (define prog-line
    (list (alg-code "prog") eqls (alg-code "defn** expr")  (blank)))
  (define defn-line
    (list (alg-code "defn") eqls (alg-code "id(id) = expr")  (blank)))

  (define prog+defn-lines
    (append prog-line defn-line))

  (define (expr-lines a)
    (let ([arrow (colorize (arrow (* 3/4 font-size) (- pi)) RedColor)])
      (list (alg-code "expr") eqls (alg-code "(expr + expr)") (if (= a 0) arrow (ghost arrow))
	    (blank)           -or- (alg-code "(expr - expr)") (if (= a 1) arrow (blank))
	    (blank)           -or- (alg-code "id(expr)") (if (= a 2) arrow (blank))
	    (blank)           -or- (alg-code "id | num") (if (>= a 3) arrow (blank)))))

  (define id-line
    (list (alg-code "id")   eqls (hbl-append (colorize (t "a variable name: ") MetaColor)
					     (alg-code "f,, x,, y,, z,, ")
					     (colorize (t "...") MetaColor))  (blank)))
  
  (define num-line
    (list (alg-code "num")    eqls (hbl-append (colorize (t "a number: ") MetaColor)
					       (alg-code "1,, 42,, 17,, ")
					       (colorize (t "...") MetaColor)) (blank)))

  (define id+n-lines
    (append id-line num-line))

  (define (grammar-table l)
    (table
     4
     l
     lbl-superimpose lbl-superimpose
     font-size line-sep))

  (define complete-table
    (grammar-table
     (append
      prog+defn-lines
      (expr-lines -1)
      (list (blank) (blank) (blank)  (blank))
      id+n-lines)))

  (define (two-line-reduction l1 l2 l2x)
    (ht-append
     font-size
     (vl-append
      line-sep
      l1
      l2)
     (alg-code " -> ")
     (vl-append
      line-sep
      l1
      l2x)))

  (define (beta*-reduction val? free?)
    (let ([val (if val? "val" "expr")])
      (vc-append
       (/ font-size 2)
       (let ([top (alg-code "... (define (id_0 id_1....id_k) expr_a) ...")])
	 (vc-append
	  (/ font-size 2)
	  (vl-append
	   line-sep
	   top
	   (alg-code (format "... (id_0 ~a_1....~a_k) ..." val val)))
	  (alg-code "->")
	  (vl-append
	   line-sep
	   top
	   (alg-code "... expr_b ..."))))
       (page-para* "where" (alg-code "expr_b") "is" (alg-code "expr_a") 
		   "with" (if free? (background YellowColor (bt "free")) "") (alg-code "id_i") 
		   "replaced by" (alg-code (format "~a_i" val))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (steps prog . l)
    (let loop ([l l])
      (cons
       (list (two-line-reduction prog (car l) (cadr l)))
       (if (null? (cddr l))
	   null
	   (loop (cdr l))))))
  
  (provide MetaColor
	   alg-patterns
	   alg-code alg-code*
	   alg-code$ alg-code$*
   
	   eqls -or-
	   prog-line defn-line prog+defn-lines
	   expr-lines
	   id-line num-line id+n-lines
	   grammar-table
	   complete-table
	   two-line-reduction
	   beta*-reduction
	   steps))

  

    