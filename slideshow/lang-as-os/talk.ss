
(module talk (lib "slideshow-run.ss" "texpict")
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "math.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
	   "mkns.ss")

  (define omit-utah-affiliation? #f)
  (define limits-as-ongoing? #f)
  (define pl-vs-os? #f)
  (define show-inspectors? #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (set-use-background-frame! #t)
  (define demo-inset (make-slide-inset 0 0 0 200))

  (define subtalk-custodian #f)
  (define subtalk-eventspace #f)
  (define subtalk-namespace #f)

  (define (end-subtalk)
    (when subtalk-custodian
      (custodian-shutdown-all subtalk-custodian))
    (set! subtalk-custodian #f)
    (set! subtalk-eventspace #f)
    (set! subtalk-namespace #f))

  (define (begin-subtalk)
    (unless subtalk-custodian
      (set! subtalk-custodian (make-custodian))
      (parameterize ([current-custodian subtalk-custodian])
	(set! subtalk-eventspace (make-eventspace))
	(parameterize ([current-eventspace subtalk-eventspace])
	  (set! subtalk-namespace (v202-make-namespace-with-mred))
	  (queue-callback (lambda ()
			    (current-namespace subtalk-namespace)
			    (print-struct #t)
			    (set-subtalk-ports)))))))

  (define (subtalk-eval expr)
    (parameterize ([current-eventspace subtalk-eventspace])
      (queue-callback (lambda () (eval expr)))))

  (define plain-style (make-object style-delta% 'change-normal))
  (define red-it-style (send (make-object style-delta% 'change-style 'italic)
			     set-delta-foreground
			     "red"))

  (send plain-style set-delta 'change-size 24)
  (send red-it-style set-delta 'change-size 24)

  (define (set-subtalk-ports)
    (let ([ow #f]
	  [e #f])
      (let ([mk-write (lambda (style)
			(lambda (s start end nonblock? w/break?)
			  (let-values ([(s) (subbytes s start end)]
				       [(display-w display-h) (get-display-size)])
			    (unless ow
			      (set! ow (make-object frame% "stdout" #f
						    575 200 10 (- display-h 200)
						    '(no-caption no-resize-border)))
			      (set! e (make-object text%))
			      (make-object editor-canvas% ow e '(hide-hscroll hide-vscroll))
			      (send ow show #t))
			    (let ([orig (send e last-position)])
			      (send e begin-edit-sequence)
			      (send e insert (bytes->string/utf-8 s) orig)
			      (send e change-style style orig (send e last-position))
			      (send e end-edit-sequence)))
			  (- end start)))])
	(current-output-port (make-output-port
			      'subtalk
			      always-evt
			      (mk-write plain-style)
			      void))
	(current-error-port (make-output-port
			     'subtalk
			     always-evt
			     (mk-write red-it-style)
			     void)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define implies (text "\336" 'symbol font-size))

  (define-syntax (with-steps stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr)
       (let ([capturing (lambda (s)
			  (datum->syntax-object #'expr s))])
	 (with-syntax ([after (capturing 'after)]
		       [vafter (capturing 'vafter)]
		       [between (capturing 'between)]
		       [vbetween (capturing 'vbetween)]
		       [between-excl (capturing 'between-excl)]
		       [vbetween-excl (capturing 'vbetween-excl)])
	   #'(let ([steps '(step-name ...)])
	       (map (lambda (step)
		      (define (after p)
			(memq step (or (memq p steps) null)))
		      (define (vafter p)
			(if (after p) values ghost))
		      (define (between p1 p2)
			(and (after p1) (or (eq? step p2) (not (after p2)))))
		      (define (vbetween p1 p2)
			(if (between p1 p2) values ghost))
		      (define (between-excl p1 p2)
			(and (after p1) (not (after p2))))
		      (define (vbetween-excl p1 p2)
			(if (between-excl p1 p2) values ghost))
		      expr)
		    steps))))]))

  (define tt-size
    (case font-size
      [(24) 20]
      [else 24]))
  
  (define (xtt s)
    (text s `(bold . modern) tt-size))
  (define (xtt* . x)
    (apply vl-append line-sep (map xtt x)))

  (define (half-page-item . s)
    (apply item (* client-w 3/4) s))

  (define brite-color "purple")
  (define dim-color "dark gray")
  (define hilite-color "black")

  (define (make-link s)
    (let ([p (t s)])
      (lift
       (colorize
	(vc-append
	 p
	 (linewidth 2 (hline (pict-width p) 1)))
	"blue")
       (- (+ 2 (pict-descent p))))))

  (define eval-link (make-link "eval"))
  
  (define (demo-slide/title s thunk . x)
    (slide/title/inset
     s
     demo-inset
     (lbl-superimpose
      (ct-superimpose
       (apply-slide-inset demo-inset titleless-page)
       (apply
	vl-append
	font-size
	x))
      (clickback eval-link thunk))))

  (define (mk-code-eval code)
    (lambda ()
      (begin-subtalk)
      (subtalk-eval (read (open-input-string 
			   (format "(begin ~a)"
				   (apply string-append code)))))))

  (define eval-code-slide
    (lambda (title . code)
      (demo-slide/title
       title
       (mk-code-eval (filter string? code))
       (apply tt*/normal code))))

  (define (eval-code-block/sep sep . code)
    (hbl-append
     sep
     (apply tt*/normal code)
     (clickback
      eval-link
      (mk-code-eval (filter string? code)))))

  (define (eval-code-block . code)
    (apply eval-code-block/sep font-size code))

  (define (tt*/dimmer dimmer l)
    (apply
     vl-append
     line-sep
     (let loop ([l l])
       (cond
	[(null? l) null]
	[(or (null? (cdr l)) (string? (cadr l)))
	 (cons (dimmer (xtt (car l))) (loop (cdr l)))]
	[else
	 (cons (colorize (let loop ([s (car l)])
			   (cond 
			    [(number? (cadr l))
			     (let ([len (string-length s)]
				   [out (if (negative? (cadr l))
					    dimmer
					    values)]
				   [in (if (negative? (cadr l))
					   values
					   dimmer)]
				   [n (abs (cadr l))])
			       (hbl-append 
				(in (xtt (substring s 0 (- len n))))
				(out (xtt (substring s (- len n) (- len (sub1 n)))))
				(in (xtt (substring s (- len (sub1 n)) len)))))]
			    [(eq? (cadr l) '<<) (xtt s)]
			    [(and (pair? (cadr l)) (string? (caadr l)))
			     (let ([m (regexp-match-positions (caadr l) s)])
			       (if m
				   (hbl-append
				    (xtt (substring s 0 (caar m)))
				    (colorize (xtt (substring s (caar m) (cdar m))) brite-color)
				    (loop (substring s (cdar m) (string-length s))))
				   (xtt s)))]
			    [else (error "bad dimmer")]))
			 hilite-color)
	       (loop (cddr l)))]))))

  (define (tt*/dim . l)
    (tt*/dimmer (lambda (x) (colorize x dim-color)) l))

  (define (tt*/normal . l)
    (tt*/dimmer values l))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (halo w h)
    (dc (lambda (dc x y)
	  (let ([p (send dc get-pen)]
		[b (send dc get-brush)])
	    (send dc set-pen (find-pen "yellow" 4))
	    (send dc set-brush (find-brush "white" 'transparent))
	    (send dc draw-ellipse (+ x 2) (+ y 2) (- w 4) (- h 4))
	    (send dc set-brush b)
	    (send dc set-pen p)))
	w h 0 0))

  (define (mk-scheme-angel-file lambda?)
    (ht-append (angel-wing 55 80 #t)
	       (vc-append
		10
		(halo 75 25)
		(cc-superimpose
		 (file-icon 80 115 #t #f)
		 (if lambda? 
		     (colorize (text "l" 'symbol 48) "blue")
		     (blank))))
	       (angel-wing 55 80 #f)))

  (define scheme-angel-file (mk-scheme-angel-file #t))

  (define scheme-angel-meta-file
    (let ([files (inset
		  (vl-append
		   scheme-angel-file
		   (inset scheme-angel-file font-size (/ font-size 2) 0 0))
		  (/ font-size 3))])
      (cb-superimpose
       (scale (mk-scheme-angel-file #f) 3.2 3)
       files)))
  
  (define mred-logo (bitmap (build-path (this-expression-source-directory) "plt-med-shield.gif")))

  (define devil-machine (desktop-machine 1.5 '(devil)))
  (define mred-machine (desktop-machine 1.5 '(plt)))

  (define down-arrow (colorize (arrow 30 (- (/ pi 2))) "green"))
  (define downright-arrow (colorize (arrow 30 (- (/ pi 4))) "green"))
  (define downleft-arrow (colorize (arrow 30 (- (* pi 3/4))) "green"))

  (define ->pt (lambda (l)
		 (map (lambda (p)
			(make-object point% (car p) (cadr p)))
		      l)))

  (define (mk-bincomm s)
    (dc (lambda (dc x y)
	  (let-values ([(sx sy) (send dc get-scale)]
		       [(dx dy) (send dc get-origin)]
		       [(op) (send dc get-pen)]
		       [(ob) (send dc get-brush)])
	    (send dc set-origin (+ dx (* sx x)) (+ dy (* sy y)))
	    (send dc set-scale (* sx s) (* sy s))

	    (send dc set-brush
		  (send the-brush-list
			find-or-create-brush
			"yellow"
			'solid))
	    (send dc set-pen
		  (send the-pen-list
			find-or-create-pen
			"black"
			0
			'transparent))

	    (send dc draw-polygon
		  (->pt '((0 10)
			  (10 0)
			  (20 10)))
		  0 0)

	    (send dc draw-polygon
		  (->pt '((0 10)
			  (10 0)
			  (20 10)))
		  100 0)

	    (let ([r (make-object region% dc)]
		  [r2 (make-object region% dc)])
	      (send r set-ellipse 5 -50 110 110)
	      (send r2 set-ellipse 15 -40 90 90)
	      (send r subtract r2)
	      (send dc set-clipping-region r)
	      (send dc draw-rectangle 5 10 110 110)
	      (send dc set-clipping-region #f))
	    
	    (let ([s "1010101010"])
	      (send dc set-font (send the-font-list find-or-create-font
				      8 'modern 'normal 'normal))
	      (let-values ([(w h a d) (send dc get-text-extent s)])
		(send dc draw-text s (/ (- 120 w) 2) (- 65 h 4))))

	    (send dc set-pen op)
	    (send dc set-brush ob)

	    (send dc set-origin dx dy)
	    (send dc set-scale sx sy)))
	(* s 120) (* s 65) 0 0))

  (define (mk-dircomm s)
    (dc (lambda (dc x y)
	  (let-values ([(sx sy) (send dc get-scale)]
		       [(dx dy) (send dc get-origin)]
		       [(op) (send dc get-pen)]
		       [(ob) (send dc get-brush)])
	    (send dc set-origin (+ dx (* sx x)) (+ dy (* sy y)))
	    (send dc set-scale (* sx s) (* sy s))

	    (send dc set-brush
		  (send the-brush-list
			find-or-create-brush
			"yellow"
			'solid))
	    (send dc set-pen
		  (send the-pen-list
			find-or-create-pen
			"black"
			0
			'transparent))

	    (send dc draw-polygon
		  (->pt '((10 0)
			  (0 10)
			  (10 20)))
		  0 0)
	    (send dc draw-polygon
		  (->pt '((0 0)
			  (10 10)
			  (0 20)))
		  85)

	    (send dc draw-rectangle
		  5 5 80 10)

	    (let ([s "(  (x) ...)"]
		  [greek (send the-font-list find-or-create-font
			       8 'symbol 'normal 'normal)]
		  [plain (send the-font-list find-or-create-font
			       6 'modern 'normal 'normal)])
	      (send dc set-font plain)
	      (let*-values ([(w h d a) (send dc get-text-extent s)]
			    [(lw lh ld la) (send dc get-text-extent "l" greek)]
			    [(maxa) (max (- h d) (- lh ld))]
			    [(ss) (/ (- 20 (max h lh)) 2)])
		(send dc draw-text s (/ (- 90 w) 2) (+ ss (- maxa (- h d))))
		(send dc set-font greek)
		(send dc draw-text "l" (+ (/ (- 90 w) 2) lw) (+ ss (- maxa (- lh ld))))))

	    (send dc set-pen op)
	    (send dc set-brush ob)
	    (send dc set-origin dx dy)
	    (send dc set-scale sx sy)))
	(* s 95) (* s 20) 0 0))

  (define bincomm (mk-bincomm 3))
  (define dircomm (mk-dircomm 3))

  (define (center-wrt a p)
    (cc-superimpose p (blank (pict-width a) 1)))
    
  (define together-arrows
    (hc-append
     (pict-width dircomm)
     downright-arrow
     downleft-arrow))
    
  (define straight-arrows
    (hc-append
     font-size
     (center-wrt scheme-angel-file down-arrow)
     (ghost (launder dircomm))
     (center-wrt scheme-angel-file down-arrow)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/center
   (vc-append
    line-sep
    (titlet "Programming Languages as Operating Systems"))
   (blank)
   (bitmap (build-path (collection-path "icons") "PLTnolarval.jpg"))
   (blank)
   (vc-append
    line-sep
    (colorize (t "Matthew Flatt") "blue")
    (blank font-size)
    ((if omit-utah-affiliation? ghost values) (t "University of Utah"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (label-left s p)
    (let ([t (text s `(bold . ,main-font) font-size)]
          [sep (/ font-size 2)])
      (inset (hc-append sep p t) 0 0 (- 0 sep (pict-width t)) 0))) 
  
  (with-steps
   (angel devil mred angels angels-run bincomm multi-mred dircomm mredos)
   (slide/title
    (cond
      [(between 'angel 'mred) "Programming in Heaven"]
      [(between 'angels 'bincomm) "Multi-Programming"]
      [(between 'multi-mred 'mredos) "Multi-Programming in Heaven"])  
    (cc-superimpose
     (vc-append
      (/ font-size 2)
      (hc-append
       font-size
       ((vafter 'angels) scheme-angel-file)
       (cc-superimpose
	((vbetween-excl 'angel 'angels) scheme-angel-file)
	((vafter 'dircomm) dircomm))
       ((vafter 'angels) scheme-angel-file))
      (cc-superimpose
       ((vbetween-excl 'mred 'angels) down-arrow)
       ((vbetween-excl 'angels-run 'multi-mred) straight-arrows)
       ((vafter 'multi-mred) together-arrows))
      (hc-append
       font-size
       ((vbetween-excl 'angels-run 'multi-mred) (center-wrt scheme-angel-file mred-logo))
       (cc-superimpose
	((vbetween 'mred 'mred) (center-wrt dircomm (label-left "language run-time" mred-logo)))
	((vafter 'mredos) (label-left "language as OS" mred-machine))
	((vbetween-excl 'multi-mred 'mredos) (center-wrt dircomm mred-logo)))
       ((vbetween-excl 'angels-run 'multi-mred) (center-wrt scheme-angel-file mred-logo)))
      (cc-superimpose
       ((vbetween 'mred 'mred) down-arrow)
       ((vbetween-excl 'multi-mred 'mredos) down-arrow)
       ((vbetween-excl 'angels-run 'multi-mred) together-arrows))
      ((vbetween-excl 'devil 'mredos) devil-machine))
     ((vbetween 'bincomm 'bincomm) bincomm))))

  (define as-os-bullets
    (vc-append
     font-size
     (page-para "Language as OS"  implies "process controls:")
     (blank)
     (vl-append
      font-size
      (page-item* "Separate threads of evaluation")
      (page-item* "Separate process-specific state (e.g., current directory)")
      (page-item* "Separate graphical event loops")
      (page-item* "Ability to terminate a process and reclaim its resources"))))
    
  (slide/title/center
   "Languages as Operating Systems"
   as-os-bullets
   'next
   (blank)
   (colorize
    (vl-append
     gap-size
     (page-para "Language-based OSes: Pilot [Redell80], SPIN [Bershad95], ...")
     (page-para "Extended languages: JKernel [Hawblitzel98], Alta [Tullman99], KaffeOS [Back00]," " ..."))
    blue))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (file-label p s)
    (vc-append
     line-sep
     p
     (bt s)))
  
  (slide/title
   "Example: Processes in a Language"
   (blank)
   (vc-append
    (/ font-size 2)
    (hc-append
     (file-label scheme-angel-file "DrScheme")
     (ghost dircomm)
     (file-label scheme-angel-file "user's program"))
    (blank)
    together-arrows
    mred-machine)
   (blank)
   (page-para
    (clickback
     (make-link "Run DrScheme")
     (lambda ()
       (end-subtalk)
       (begin-subtalk)
       (subtalk-eval
        `(parameterize ([current-command-line-arguments (vector "demo.scm")])
           (exit-handler (lambda (x) (,end-subtalk)))
           (dynamic-require '(lib "drscheme.ss" "drscheme") #f)))))))
   
  (define prog-process-title "More Process Examples")

  (slide/title/center
   prog-process-title 
   (bitmap "installer.bmp"))

  '(slide/title/center
    prog-process-title   
    (bitmap "browser.bmp"))

  (slide/title/center
   prog-process-title
   (let ([ws (bitmap "web-server.gif")]
	 [ie (bitmap "ie.bmp")])
     (let ([ies (map launder (list ie ie ie ie))])
       (let ([p (vc-append
		 (* 2 (pict-height ws))
		 ws
		 (apply
		  hc-append
		  (* 2 (pict-width ie))
		  ies))])
	 (foldl (lambda (ie p)
		    (add-line p
			      ws find-cb
			      ie find-ct
			      1 dim-color))
		p ies)))))
       
   
  (slide/title/center
   prog-process-title
   (bitmap "sirmail.bmp"))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|
  
  (define (big s)
    (text s `(bold . ,main-font) (floor (* #e1.5 font-size))))

  (define (mk= on?)
    (slide/title/center
     "Premise"
     (big "programming language")
     (big "=")
     (big "operating system")
     (blank)
     (blank)
     ((if on? values ghost)
      (colorize (t "formal semantics for either is a virtual machine") "blue"))))
  
  (mk= #f)
  (mk= #t)

  (define windows-icon (bitmap "windows.gif"))
  (define macos-icon (bitmap "macos.gif"))
  (define linux-icon (bitmap "linux.gif"))

  (define java-icon (bitmap "java.gif"))
  (define python-icon (bitmap "python.gif"))
  (define plt-icon (bitmap "plt.gif"))

  (define (lbl-icon icon name)
    (hc-append (/ font-size 2) icon (bt name)))

  (slide/title/center
   "Operating Systems vs Programming Languages"
   (hc-append
    (* 4 font-size)
    (vc-append
     (* 4 font-size)
     (lbl-icon windows-icon "Windows")
     (inset (lbl-icon macos-icon "Mac OS") 0 0 (* 3 font-size) 0)
     (lbl-icon linux-icon "Linux"))
    
    (vc-append
     (* 4 font-size)
     (lbl-icon java-icon "Java")
     (inset (lbl-icon python-icon "Python") (* 3 font-size) 0 0 0)
     (lbl-icon plt-icon "PLT Scheme"))))
  
  |#
  
  (define spectrum
    (let* ([w (inexact->exact (floor (* client-w 3/4)))]
	   [bm (make-object bitmap% w font-size)]
	   [dc (make-object bitmap-dc% bm)])
      (let ([c (make-object color%)]
	    [p (send dc get-pen)])
	(let loop ([i 0])
	  (send c set 
		(inexact->exact (floor (* 255 (/ i w))))
                0
		(inexact->exact (floor (* 255 (/ (- w i) w)))))
	  (send dc set-pen (find-pen c))
	  (send dc draw-line i 0 i font-size)
	  (unless (= i w)
	    (loop (add1 i))))
	(send dc set-pen p))
      (send dc set-bitmap #f)
      (bitmap bm)))

  (define (bbt s)
    (text s `(bold . ,main-font) (floor (inexact->exact (* 1.5 font-size)))))

  (when pl-vs-os?
    (with-steps
     (ospl bias spectrum move)
     (slide/title/center
      "Programming Languages vs. Operating Systems"
      (vc-append
       (page-para*/c "Languages and operating systems both virtualize the machine")
       (blank (* 3 font-size))
       (rc-superimpose
	(lc-superimpose
	 (hc-append
	  mred-logo
	  (blank (/ font-size 2))
	  ((vafter 'move) (colorize (arrow (* 2 font-size) 0) "red"))
	  ((vafter 'move) mred-machine))
	 (blank (* client-w 3/4) (* 2 font-size)))
	devil-machine)
       ((vafter 'spectrum) spectrum)
       (blank)
       (ht-append
	(* client-w #e.15)
	(para (* client-w #e.3)
	      "virtualize machine"
	      "for expressiveness")
	(para/r (* client-w #e.3) 
		"virtualize machine"
		"for non-interference"))
       (blank font-size)
       ((vafter 'bias)
	(ht-append
	 (* client-w #e.15)
	 (colorize (para (* client-w #e.3) implies (bt "cooperation")) "blue")
	 (colorize (para/r (* client-w #e.3) implies (bt "isolation")) "red")))))))
  
  (define xform-icon
    (hc-append
     mred-logo
     (blank (/ font-size 2))
     (colorize (arrow (* 2 font-size) 0) "red")
     mred-machine))
  
  (when pl-vs-os?
    (slide/title
     "Turning a Language into an Operating System"
     (blank)
     xform-icon
     (blank)
     (page-para "Maintain bias toward cooperation:")
     (page-item "Language safety enforces abstractions")
     (page-item "Split \"process\" into multiple abstractions")
     (blank)
     (colorize (page-para*/c "Programmer chooses degree of isolation"
			     "to meet specific needs")
	       "blue")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define outline
    (apply
     make-outline 
     'motivation "Motivation and Approach" null
     'plt-scheme "PLT Scheme as an Operating System" 
     (lambda (sym)
       (apply
	vl-append
        line-sep
        (half-page-item "Threads")
        (half-page-item "Parameters")
        (half-page-item "Eventspaces")
        (half-page-item "Custodians")
	(if show-inspectors?
	    (list (half-page-item "Inspectors"))
	    null)))
     'scheme-esq "Putting the Pieces to Work"
     (lambda (sym)
       (half-page-item
        (clickback
         (make-link "SchemeEsq")
         (lambda ()
           (end-subtalk)
           (begin-subtalk)
           (subtalk-eval '(require "scheme-esq.scm"))))
        ", a mini DrScheme"
        (blank font-size)
        (colorize (t "[ICFP99]") dim-color)))
     (if limits-as-ongoing?
	 (list
	  'limits "Ongoing Research"
	  (lambda (sym)
	    (half-page-item
	     "Resource accounting")))
	 null)))

  (outline 'plt-scheme)

  (define (aspect s)
    (colorize
     (page-para* 
      ;; capitalize s:
      (it (string-append (string (char-upcase (string-ref s 0))) (substring s 1 (string-length s)))))
     "blue"))


  (slide/title/tall/inset
   "Threads"
   demo-inset
   (aspect "concurrent execution")
   (blank)
   (vl-append
    (hbl-append
     font-size
     (xtt* 
      "(require \"spin-display.scm\")")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(require "spin-display.scm")))))
    (eval-code-block
     " "
     "(define (spin)"
     "  (rotate-a-little)"
     "  (sleep 0.1)"
     "  (spin))"
     " "
     "(define spinner (thread spin))" '("thread"))
    (eval-code-block
     " "
     "(kill-thread spinner)" '("kill-thread"))))
  
  (slide/title/tall/inset
   "Parameters  (a.k.a. Fluid Variables)"
   demo-inset
   (aspect "thread-local state")
   (vl-append
    line-sep
    (eval-code-block
     "(printf \"Hello\\n\")"
     "(fprintf (current-output-port) \"Hola\\n\")" '("current-.*port")
     "(fprintf (current-error-port) \"Goodbye\\n\")" '("current-.*port")
     "(error \"Ciao\")")
    (eval-code-block/sep
     0
     " "
     "(parameterize ((current-error-port (current-output-port)))" '("parameterize")
     "  (error \"Au Revoir\"))")
    (eval-code-block/sep
     0
     " "
     "(parameterize ((current-error-port (current-output-port)))"  '("parameterize")
     "  (thread"  '("thread")
     "    (lambda ()"
     "      (error \"Zai Jian\"))))")))
   
  (slide/title/tall/inset
   "Eventspaces"
   demo-inset
   (aspect "concurrent GUIs")
   (blank)
   (vl-append
    line-sep
    (eval-code-block
     "(thread (lambda () (message-box \"One\" \"Hi\")))"
     "(thread (lambda () (message-box \"Two\" \"Bye\")))")
    (eval-code-block
     " "
     " "
     "(thread (lambda () (message-box \"One\" \"Hi\")))"
     "(parameterize ((current-eventspace (make-eventspace)))" '("(make|current)-eventspace")
     "  (thread (lambda () (message-box \"Two\" \"Bye\"))))")))

  (slide/title/tall/inset
   "Custodians"
   demo-inset
   (aspect "termination and clean-up")
   (blank)
   (vl-append
    line-sep
    (hbl-append
     font-size
     (tt*/normal
      "(define c (make-custodian))" '("make-custodian")
      "(parameterize ((current-custodian c))" '("current-custodian")
      "  ...)")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(define c (make-custodian)))
	(subtalk-eval '(parameterize ((current-custodian c))
			 (parameterize ((current-eventspace (make-eventspace)))
			   (dynamic-require "start-a-lot.scm" #f)))))))
    (eval-code-block
     " "
     "(custodian-shutdown-all c)" '("cust.*all"))))
   
  (slide/title/tall/inset
   "Custodians"
   demo-inset
   (aspect "resource limits")
   (blank)
   (vl-append
    line-sep
    (hbl-append
     font-size
     (tt*/normal
      "(define (run-away)"
      "  (cons 1 (run-away)))"
      " "
      "(custodian-limit-memory c 1000000 c)"  '("custodian-limit-memory")
      " "
      "(parameterize ((current-custodian c))"
      "  ..."
      "  (thread run-away))")
     (clickback
      eval-link
      (lambda ()
	(end-subtalk)
	(begin-subtalk)
	(subtalk-eval '(define c (make-custodian)))
	(subtalk-eval '(begin
			 (custodian-limit-memory c 1000000 c)
			 (parameterize ((current-custodian c))
			   (parameterize ((current-eventspace (make-eventspace)))
			     (dynamic-require "start-a-lot.scm" #f)
			     (thread
			      (lambda ()
				(let loop ([n 0]) 
				  (unless (= n 500000) 
				    (add1 (loop (add1 n))))))))))))))))
   
  (when show-inspectors?
    (slide/title/tall/inset
     "Inspectors"
     demo-inset
     (aspect "reflection and debugging access")
     (blank)
     (vl-append
      line-sep
      (eval-code-block
       "(define-struct fish (color weight))"
       "(define eddie (make-fish 'red 100))"
       "(print eddie)")
      (eval-code-block
       " "
       " "
       "(define senior-inspector (current-inspector))" '("(senior-insepctor)|(current-inspector)")
       "(parameterize ([current-inspector (make-inspector)])"  '("make-inspector")
       "  (define-struct fish (color weight))"
       "  (define eddie (make-fish 'red 100))"
       "  (parameterize ([current-inspector senior-inspector])"  '("senior-inspector")
       "    (print eddie)))"))))

  (slide/title
   "Etc."
   (half-page-item "Security Guards")
   (aspect "resource access control")
   (half-page-item "Namespaces")
   (aspect "global bindings")
   (half-page-item "Will Executors")
   (aspect "timing of finalizations")
   'alts
   (if show-inspectors?
       null
       (list (list
	      (half-page-item "Inspectors")
	      (aspect "debugging access")))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (outline 'scheme-esq)

  (demo-slide/title
   "GUI - Frame"
   (lambda ()
     (end-subtalk)
     (begin-subtalk)
     (subtalk-eval 
      '(begin 
	 (define frame (new frame% [label "SchemeEsq"] [width 400] [height 185]))
	 (send frame show #t))))
   (tt*/normal
    "(define frame"
    "  (new frame% " '("frame%")
    "    [label \"SchemeEsq\"]"
    "    [width 400] [height 175]))"
    ""
    "(send frame show #t)"))

  (eval-code-slide
   "GUI - Reset Button"
   "(new button%" '("button%")
   "  [label \"Reset\"]"
   "  [parent frame]"
   "  [callback (lambda (b e) (reset-program))])")

  (eval-code-slide
   "GUI - Interaction Area"
   "(define repl-display-canvas"
   "  (new editor-canvas%" '("editor-canvas%")
   "    [parent frame]))")

  (demo-slide/title
   "GUI - Interaction Buffer"
   (lambda ()
     (subtalk-eval '(begin
		      (load "text.ss")
		      (define repl-editor (make-object esq-text%))
		      (send repl-display-canvas set-editor repl-editor))))
   (tt*/normal
    "(define esq-text% "
    "  (class text% ... (evaluate str) ...))" '("text%")
    " "
    "(define repl-editor (new esq-text%))"
    "(send repl-display-canvas set-editor repl-editor)"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (eval-code-slide
   "Evaluator"
   "(define (evaluate expr-str)"
   "  (thread" '("thread")
   "    (lambda ()"
   "      (print (eval (read (open-input-string expr-str))))" '("(read)|(eval)|(print)")
   "      (newline)"
   "      (send repl-editor new-prompt))))")

  (demo-slide/title
   "Evaluator Output"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-output-port
	   (make-custom-output-port
	    #f
	    (lambda (s start end nonblock?) 
	      (send repl-editor output (substring s start end))
	      (- end start))
	    void void))

	 (define (evaluate expr-str)
	   (thread
	    (lambda ()
	      (current-output-port user-output-port)
	      (with-handlers ((exn? 
			       (lambda (exn)
				 (display (exn-message exn)))))
		(print (eval (read (open-input-string expr-str)))))
	      (newline)
	      (send repl-editor new-prompt)))))))
   (tt*/dim
      "(define user-output-port" '<<
      "  (make-custom-output-port ... repl-editor ...))" '<<
      " "
      "(define (evaluate expr-str)"
      "  (parameterize ((current-output-port user-output-port))" '<<
      "    (thread"
      "      (lambda ()"
      "        ...))))" '+2))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (demo-slide/title
   "Evaluating GUIs"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-eventspace (make-eventspace))
	 
	 (define (evaluate expr-str)
	   (thread
	    (lambda ()
	      (current-output-port user-output-port)
	      (current-eventspace user-eventspace)
	      (with-handlers ((exn? 
			       (lambda (exn)
				 (display (exn-message exn)))))
		(print (eval (read (open-input-string expr-str)))))
	      (newline)
	      (send repl-editor new-prompt)))))))
   (vl-append
    (tt*/dim
     "(define user-eventspace (make-eventspace))" '<<
     " "
     "(define (evaluate expr-str)"
     "  (parameterize ((current-output-port user-output-port)"
     "                 (current-eventspace user-eventspace))" -1
     "    (thread"
     "      (lambda ()"
     "        ...)))")))

  (demo-slide/title
   "Custodian for Evaluation"
   (lambda ()
     (subtalk-eval
      '(begin
	 (define user-custodian (make-custodian))
	 (define user-eventspace
	   (parameterize ((current-custodian user-custodian))
	     (make-eventspace)))
	 (define (evaluate expr-str)
	   (parameterize ((current-custodian user-custodian))
	     (thread
	      (lambda ()
		(current-output-port user-output-port)
		(current-eventspace user-eventspace)
		(with-handlers ((exn? 
				 (lambda (exn)
				   (display (exn-message exn)))))
		  (print (eval (read (open-input-string expr-str)))))
		(newline)
		(send repl-editor new-prompt))))))))
   (tt*/dim
    "(define user-custodian (make-custodian))" '<<
    " "
    "(define user-eventspace" '<<
    "  (parameterize ((current-custodian user-custodian))" '<<
    "    (make-eventspace)))" '<<
    " "
    "(define (evaluate expr-str)" 
    "  (parameterize ((current-output-port user-output-port)"
    "                 (current-eventspace user-eventspace)"
    "                 (current-custodian user-custodian))" -1
    "    (thread"
    "      (lambda ()"
    "        ...))))"))

  (eval-code-slide
   "Reset Evaluation"
   "(define (reset-program)"
   "  (custodian-shutdown-all user-custodian)"
   " "
   "  (set! user-custodian (make-custodian))"
   "  (parameterize ((current-custodian user-custodian))"
   "    (set! user-eventspace (make-eventspace)))"
   "  (send repl-editor reset))")

  (when show-inspectors?
    (demo-slide/title
     "Inspecting Results"
     (lambda ()
       (subtalk-eval
	'(begin
	   (define esq-inspector (current-inspector))
	   (define user-inspector (make-inspector))
	   (define (evaluate expr-str)
	     (parameterize ((current-custodian user-custodian)
			    (current-inspector user-inspector))
	       (thread
		(lambda ()
		  (current-output-port user-output-port)
		  (current-eventspace user-eventspace)
		  (with-handlers ((exn? 
				   (lambda (exn)
				     (display (exn-message exn)))))
		    (let ([v (eval (read (open-input-string expr-str)))])
		      (parameterize ((current-inspector esq-inspector))
			(print v))))
		  (newline)
		  (send repl-editor new-prompt))))))))
     (tt*/dim
      "(define esq-inspector (current-inspector))" '<<
      "(define user-inspector (make-inspector))" '<<
      " "
      "(define (evaluate expr-str)" 
      "  (parameterize (..."
      "                 (current-inspector user-inspector))" -1
      "    (thread"
      "      (lambda ()"
      "        (let ([v (eval ...)])" '<<
      "          (parameterize ((current-inspector esq-inspector))" '<<
      "            (print v))" '<<
      "        ...))))")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when limits-as-ongoing?
    (outline 'limits))

  (define (mk-drs-layout s)
    (let* ([user-file (ghost scheme-angel-file)]
	   [main
	    (vc-append
	     (/ font-size 2)
	     (hc-append
	      (file-label scheme-angel-file "DrScheme")
	      (ghost dircomm)
	      (file-label user-file "user's program"))
	     (blank)
	     together-arrows
	     mred-machine)])
      (let-values ([(x y) (find-lb main user-file)])
	(cons-picture
	 main
	 `((place ,x ,y
		  ,(let* ([u scheme-angel-file]
			  [big (scale u s s)])
		     (inset big 
			    (/ (- (pict-width u) (pict-width big)) 2)
			    (/ (- (pict-height u) (pict-height big)) 2)))))))))

  (when limits-as-ongoing?
    (map
     (lambda (s)
       (slide/title/center
	"Resource Consumption"
	(mk-drs-layout s)))
     '(1 1.25 1.5 2 3 4)))

  (define object-color "forest green")
  (define (mk-object size)
    (cloud (* size 1.5 font-size) (* size font-size) object-color))
  
  (define (mk-partition-icon objs)
    (frame (inset (hc-append 
		   font-size
		   scheme-angel-file
		   objs)
		  (/ font-size 2))))
  
  (when limits-as-ongoing?
    (slide/title
     "Resource Accounting"
     (page-item (colorize (bt "Conventional OS") "red") ": process memory use = size of partition")
     (page-subitem "Accounting is easy")
     (page-subitem "Trading data is difficult")
     (blank)
     (hc-append
      font-size
      (mk-partition-icon (vc-append
			  (mk-object 2)
			  (hc-append (mk-object 1) (mk-object 1.2))))
      (mk-partition-icon (vc-append
			  (hc-append
			   (vc-append (mk-object 1) (mk-object 1.2))
			   (mk-object 2))
			  (mk-object 1.3))))))

  (define (connect-all p o . tos)
    (let loop ([p p][tos tos])
      (if (null? tos)
	  p
	  (loop
	   (add-line p o find-cb
		     (car tos) find-ct
		     1
		     object-color)
	   (cdr tos)))))

  (define memory-icon
    (let* ([a1 (launder scheme-angel-file)]
	   [a2 (launder scheme-angel-file)]
	   [o1 (mk-object 1)]
	   [o2 (mk-object 1.2)]
	   [o3 (mk-object 1.3)]
	   [o4 (mk-object 1)]
	   [o5 (mk-object 1.4)]
	   [o6 (mk-object 1.25)]
	   [p (vc-append
	       font-size
	       (hc-append (* 0.5 (pict-width a1)) a1 a2)
	       (blank)
	       (hc-append font-size (ghost (launder o3)) o1 (blank) o2)
	       (blank)
	       (hc-append font-size o5 (ghost (launder o1)) o3 (ghost (launder o2)) o4 o6))])
      (connect-all (connect-all (connect-all (connect-all p a1 o1 o5) a2 o1 o2) o2 o6 o4) o1 o3)))

  (when limits-as-ongoing?
    (slide/title
     "Resource Accounting"
     (page-item (colorize (bt "Language as OS") "blue") ": process memory use = size of owned data")
     (page-subitem "Trading data is easy")
     (page-subitem "Accounting is difficult: sharing, real-time tracking")
     memory-icon)
    
    (slide/title
     "Resource Accounting Strategy"
     (page-para "General strategy: compute accounting charges during GC")
     memory-icon
     (blank)
     (colorize
      (page-para* "Exact accounting is"
		  (hbl-append (t "O(N") (text "2" `(superscript . , main-font) font-size) (t ")"))
		  "in the worst case...")
      "red"))

    (slide/title
     "Compromise Accounting for Sharing"
     (page-para "1. Arrange processes in a hierarchy")
     (scale scheme-angel-meta-file 0.75 0.75)
     (page-para "2. Charge parent-child sharing to child")
     (page-para "3. Charge child-child sharing to random child"))

    (slide/title
     "Compromise Accounting for Sharing"
     (page-para "DrScheme experience with compromise Accounting:")
     (page-item "Required small changes to DrScheme (< 20 lines)")
     (page-item "Correct program killed when running multiple good user programs and one bad program")
     'NEXT
     (blank)
     (colorize (page-para* "Still conducting experiments and exploring policies") "red")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (slide/title
   "Conclusion"
   xform-icon
   (page-item "Programmers need OS-like constructs in languages")
   (vc-append
    line-sep
    (page-subitem "concurrency")
    (page-subitem "adjust run-time environment")
    (page-subitem "easy termination"))
   (blank)
   (page-item "Multiple language constructs for \"process\"")
   (page-subitem "programmer can mix and match to"
                 "balance isolation and cooperation"))

  
  'done)
