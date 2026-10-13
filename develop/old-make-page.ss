(require (lib "xml.ss" "xml"))

(empty-tag-shorthand html-empty-tags)

(define (link s)
  `(a ((href ,s)) ,s))

(define TBA "TBA")
(define None "None")
(define None/Incl200 None)

(define 53+ "53 to 206")
(define 53-103 "53 to 103")
(define 53+? "53 (to 103?)")
(define 100+ "100 to 103")
(define 100++ "100 to 204")
(define 101+ "101 to 103")
(define 101++ "101 to 206")
(define 102+ "102 to 103")
(define 103= "103")
(define 103+ "103")
(define 103++ "103 to 206")
(define 200+ "200 to 206")
(define 202+ "202 to 206")
(define 203+ "203 to 206")
(define 200-203 "200 to 203")
(define 204+ "204 to 206")
(define 205+ "205 to 206")
(define 206+ "205")

(define (included-in-latest v v2)
  (seq v ", "
       `(a ((href "in-v200.html"))
	   ,v2)))

(define (included/200 v)
  (included-in-latest v 200+))
(define (included/204 v)
  (included-in-latest v 204+))

(define Windows "Windows")
(define All "All")

(define Included "included with PLT Scheme")

(define PLT '(a ((href "http://www.plt-scheme.org/"))
		"PLT"))

(define (at-Schematics name)
  `(a ((href "http://sourceforge.net/projects/schematics/"))
      ,name))

(define Schematics (at-Schematics "Schematics"))

(define (plt-page name)
  `(a ((href ,(format "http://www.plt-scheme.org/software/~a/" 
		      (list->string
		       (map char-downcase
			    (string->list name))))))
      ,name))

(define (collection name)
  `(a ((href "collections.html"))
      (tt ,name) " collection"))

(define (mailto addr)
  `(em ,addr))

(define (contact addr)
  `(font ((size "-1"))
	 nbsp
	 nbsp
	 "Contact: "
	 (em ,addr)))

(define (srfi n)
  `(font ()
	 (a ((href ,(format "http://srfi.schemers.org/srfi-~a/srfi-~a.html" n n)))
	    ,(format "SRFI ~a" n))
	 " implementation"))
       
(define head-style '((bgcolor "#ddffdd")))
(define supported '((bgcolor "#ddddff")))
(define supported-incl '((bgcolor "#ccccee")))
(define contrib supported)

(define-struct section (name))
(define (section s) (make-section s))

(define (seq . l)
  `(font () ,@l))

(define (in-progress p)
  (seq p ", "
       '(em "development in progress")))

(define packages
  `(

    ,(section "Connectivity")
    
    (,supported
     "COM, ActiveX, DCOM, OCX"
     ,(plt-page "MysterX")
     "COM/ActiveX control"
     ,103++
     ,Windows
     ,None
     ,PLT)
    
    (,contrib
     ".NET"
     (a ((href "http://www.rivendell.ws/dot-scheme/"))
        "dot-scheme")
     ".NET binding"
     ,205+
     "Windows"
     ,None
     "Pedro Pinto")

    (,supported
     "ODBC"
     ,(plt-page "SrPersist")
     "ODBC connectivity"
     ,103++
     ,All
     "ODBC Driver Manager"
     ,PLT)
    
    (,contrib
     "SQL"
     ,(at-Schematics "SchemeQL")
     "a database language layer atop SrPersist"
     ,(seq 200+
	   ", "
	   `(a ((href "ftp://wish.com.mx/pub/scheme/v103/schemeql.plt"))
	       ,103+))
     ,All
     "SrPersist"
     "Francisco Solsona")

    (,contrib
     "gdbm"
     (a ((href "mzgdbm.tar.gz"))
	"mzgdbm")
     "gdbm interface"
     ,103=
     ,All
     "C compiler"
     "Alex Ott")

    (,contrib
     "OLE/DB"
     (a ((href "http://www.rivendell.ws/ss-db/"))
	"SS-DB")
     "OLE/DB access"
     ,200+
     "Windows"
     ".NET"
     "Pedro Pinto")

    (,contrib
     "XML"
     (a ((href "http://pair.com/lisovsky/download/sxml/plt/"))
	"SSAX-SXML")
     "SSAX XML parsing framework"
     ,200+
     ,All
     ,None
     ,(seq "Oleg Kiselyov et al."))
    
    (,supported-incl
     "XML"
     ,(collection "xml")
     "Simple XML generation and parsing"
     ,101++
     ,All
     ,Included
     ,PLT)

    (,contrib
     "HTML"
     (a ((href "http://www.neilvandyke.org/htmlprag/"))
	"HtmlPrag")
     "Permissive HTML parser that emits SXML"
     ,200+
     ,All
     ,None
     (a ((href "http://www.neilvandyke.org/"))
	"Neil Van Dyke"))

    (,contrib
     "Matlab"
     (a ((href "http://www.biont.org/download/matlab-interface.plt"))
	"matlab-interface.plt")
     "Matlab interface"
     ,200+
     ,All
     "Matlab, C compiler"
     (a ((href "http://www.biont.org/"))
	"Sebastian Seidel"))

    ,(section "Networking")

    (,supported-incl
     "web server"
     ,(collection "web-server")
     "Web server"
     ,(seq 200+ ", " '(a ((href "webserver.plt")) "103"))
     ,All
     ,None
     ,PLT)

    (,supported-incl
     "CGI, HTTP, SMTP, POP, IMAP, NNTP, DNS"
     ,(collection "net")
     "CGI, HTTP, SMTP, POP, IMAP, NNTP, and DNS utilities"
     ,53+
     ,All
     ,Included
     ,PLT)

    (,contrib
     "FastCGI"
     (a ((href "ftp://wish.com.mx/pub/scheme/fastcgi-0.01-2.plt"))
	"fastcgi.plt")
     "Interface for writing FastCGI scripts in Scheme (extends Ilia's)"
     ,103=
     ,All
     ,None
     "Francisco Solsona")

    (,contrib
     "FastCGI"
     (a ((href "fastcgi/"))
	"FastCGI")
     "Interface for writing FastCGI scripts in Scheme"
     ,53+?
     ,All
     ,None
     "Ilia Perminov")

    (,contrib
     "MD5"
     (a ((href "http://www.bsd-dk.dk/~elrond/scheme/digest/"))
	"Digest")
     "MD5 implementation (RFC 1312; 128-bit fingerprint)"
     ,103=
     "see site"
     "C compiler"
     "Bertrand Petit")

    (,contrib
     "mhash"
     (a ((href "http://www.ekman.cx/software/mhash.plt"))
	"mhash.plt")
     "Bindings for the mhash library for crytographic hashes"
     ,200+
     ,All
     "C compiler, mhash lib"
     "Patrick Ekman")

    (,supported-incl
     "MIME"
     (a ((href "ftp://wish.com.mx/pub/scheme/v103/mime.plt"))
	"mime.plt")
     "MIME utilities (message analyzer)"
     ,(included/200 103=)
     ,All
     ,Included
     "Francisco Solsona")

    (,supported-incl
     "MIME"
     (a ((href "ftp://wish.com.mx/pub/scheme/v103/qp.plt"))
	"qp.plt")
     "Quoted-printable encoder/decoder (mime message bodies)"
     ,(included/200 103=)
     ,All
     ,Included
     "Francisco Solsona")

    (,contrib
     "HTTP Cookies"
     (a ((href "ftp://wish.com.mx/pub/scheme/v103/cookie.plt"))
	"cookie.plt")
     "HTTP cookies"
     ,103=
     ,All
     ,None
     "Francisco Solsona")

    ,(section "GUIs")

    (,supported-incl
     "GUI"
     ,(plt-page "MrEd")
     "GUI toolbox, drawing functions, multimedia text and graph editor"
     ,53+
     ,All
     ,Included
     ,PLT)

    (,contrib
     "Gtk"
     (a ((href "http://www.anderware.com/mzgtk/"))
	"MzGtk")
     "An interface to Gtk for MzScheme"
     ,100+
     "Unix"
     "Gtk"
     "Robert John Andersen")

    (,contrib
     "slider"
     ,(link "slider.plt")
     "2D and 1D slider widgets"
     ,100+
     ,All
     ,None
     (a ((href "http://www-pu.informatik.uni-tuebingen.de/users/sperber/"))
	"Michael Sperber"))

    (,supported-incl
     #f
     ,(collection "hierlist")
     "hierarchical list widget"
     ,53+
     ,All
     ,Included
     ,PLT)

    (,contrib
     #f
     ,(link "memory-canvas.plt")
     "A canvas for animations without flickering"
     ,203+
     ,All
     ,None
     "Katsmall the Wise")

    (,contrib
     "frame-gen"
     (a ((href "http://kingdom.multimania.com/"))
	"Frame Generator")
     "Tools for designing MrEd GUIs"
     ,101+
     ,All
     ,None
     "Benjamin Cornu")

    (,contrib
     "VScheme"
     (A ((href "http://www.vscheme.cjb.net"))
	"VScheme")
     "Visual Scheme"
     ,101+
     ,All
     ,None
     "Florian Loitsch")

    (,contrib
     "builder"
     ,(link "guibuilder.plt")
     (in-progress "A GUI for designing simple MrEd GUIs")
     ,102+
     ,All
     ,None
     ,PLT)

    ,(section "Multimedia")

    (,contrib
     "OpenGL"
     (a ((href "http://aoeu.snth.net"))
	"GLSCM")
     "OpenGL binding [more complete]"
     ,200+
     ,All
     "C compiler"
     "Robert Kooima")

    (,contrib
     "OpenGL"
     (a ((href "http://www.cs.utah.edu/~sowens/sgl.html"))
	"SGL")
     "OpenGL binding [strict error checking]"
     ,200+
     ,All
     "C compiler"
     (a ((href "http://www.cs.utah.edu/~sowens/"))
	"Scott Owens"))

    (,contrib
     "schemegl"
     (a ((href "schemegl.zip"))
	"schemegl.zip")
     "OpenGL binding"
     ,204+
     ,All
     "C compiler"
     "Olivier ORABONA")
     
    (,contrib
     "GLUTSCM"
     (a ((href "http://www.inf.ufrgs.br/~carlossch/glutscm/"))
	"GLUTSCM")
     "OpenGL Utility Toolkit (GLUT) for MzScheme"
     ,200+
     ,All
     "C compiler, GLUT"
     "Carlos Eduardo Scheidegger")

    (,contrib
     "3D"
     (a ((href "http://tuxfighter.tuxfamily.org/projects/3dscheme/index.html"))
	"3DScheme-Anim")
     "3D modeler with animations and 3D turtles"
     ,203+
     ,All
     ,None
     "Louis Parisot")

    (,contrib
     "3D"
     (a ((href "http://www.essi.fr/~merigon/projets/projets-deugMI2/projet3D/moteur3d.htm"))
	"Moteur 3D")
     "3D image redering"
     ,103=
     ,All
     ,None
     "Olivier Merigon")

    (,contrib
     "MIDI"
     ,(link "mzshare.zip")
     "MidiShare interface"
     ,100+
     ,Windows
     "midishare"
     "Jacques Herry")

    (,contrib
     "speech"
     ,(link "speech.zip")
     "An interface to the Microsoft Speech API"
     ,100+
     ,Windows
     "Speech SDK 4.0, C++ compiler"
     (a ((href "http://www.cs.unc.edu/~gb/"))
	"Gary Bishop"))
#|
    (,contrib
     "OpenGL"
     "OpenGL"
     ,(in-progress "An interface to OpenGL graphics")
     ,TBA
     ,TBA
     ,TBA
     "Greg Pettyjohn")
|#
    (,supported-incl
     "HTML, web browser"
     ,(collection "browser")
     "HTML parsing and browsing"
     ,100++
     ,All
     ,Included
     ,PLT)
    

    ,(section "Desktop Tools")

    (,contrib
     "Vim"
     (a ((href "http://iamphet.nm.ru/scheme/"))
	"MzVim")
     "Patches to Vim to embed MzScheme"
     ,205+
     ,All
     "Vim, C compiler (Unix)"
     "Sergey Khorev")

    (,contrib
     "mail client"
     ,(link "SirMail.plt")
     (in-progress "An IMAP mail client (with plenty of rough edges)")
     ,(seq 203+ ", " '(a ((href "SirMail-103.plt")) "103"))
     ,All
     ,None
     ,(seq '(a ((href "http://www.cs.utah.edu/~mflatt/")) "Flatt")
	   " " 'amp " " 
	   '(a ((href "http://www.cs.uchicago.edu/~robby/")) "Findler")))

    (,contrib
     "mnemisis"
     ,(link "mnemisis.plt")
     "Finds words to help you remember numbers"
     ,202+
     ,All
     "C compiler"
     (a ((href "http://www.brinckerhoff.org/")) "John Clements"))
    
    ,(section "Programming Tools")

    (,supported
     "analyzer"
     (a ((href "http://www.plt-scheme.org/software/mrflow/"))
	"MrFlow")
     "Static debugger and program analyzer (in progress)"
     ,200+
     ,All
     ,None
     ,PLT)

    (,contrib
     "CLOS"
     (a ((href "http://www.cs.cornell.edu/eli/Swindle/"))
        "Swindle")
     "CLOS for MzScheme"
     ,203+
     ,All
     ,None
     (a ((href "http://www.cs.cornell.edu/eli/"))
        "Eli Barzilay"))

    (,contrib
     "Quack,Emacs"
     (a ((href "http://www.neilvandyke.org/quack/"))
        "Quack")
     "Enhanced Emacs support for Scheme"
     "All"
     ,All
     "Emacs"
     (a ((href "http://www.neilvandyke.org/"))
        "Neil Van Dyke"))

    (,contrib
     "arity check"
     (a ((href "http://www.ccs.neu.edu/home/clements/download/no-brainer.plt"))
	"no-brainer.plt")
     "DrScheme tool to check for arity errors and unused bindings"
     ,203+
     ,All
     ,None
     (a ((href "http://www.brinckerhoff.org/"))
        "John Clements"))

    (,supported-incl
     "yacc,lex"
     ,(collection "parser-tools")
     "Lex and Yacc (but better) for Scheme"
     ,200+
     ,All
     ,Included
     ,PLT)

    (,contrib
     "PIC16F84,PIC16F8x MCU"
     (a ((href "http://vinvin.dyndns.org/projects/"))
	"PIC16F84")
     "Simulator for Microchip's PIC16F8x MCU"
     ,200+
     ,All
     ,None
     "Vincent Hourdin")

    (,contrib
     #f
     ,(link "algol60.plt")
     "Algol60 plug-in for DrScheme"
     ,204+
     ,All
     ,None
     ,PLT)

    (,supported-incl
     #f
     ,(collection "errortrace")
     "Simple stack-dump debugging and profiling"
     ,100++
     ,All
     ,Included
     ,PLT)

    (,contrib
     "graph"
     (a ((href "http://www.essi.fr/~merigon/projets/projets-deugMI2/projetliste/projet-liste.htm"))
	"data viewer")
     "Drawing tree data structures"
     ,103=
     ,All
     ,None
     "Olivier Merigon")

    (,contrib
     "lint"
     (a ((href "check-variables.scm"))
        "check-variables.scm")
     "A utility that checks for unbound variables"
     ,53+
     ,All
     ,None
     "Will Fitzgerald")

    (,supported-incl
     "make"
     ,(collection "make")
     ,(seq "Scheme-oriented variant of the " '(tt "make") " utility")
     ,100++
     ,All
     ,Included
     ,PLT)

    (,supported
     "analyzer"
     ,(plt-page "MrSpidey")
     "Static debugger and program analyzer"
     ,53-103
     ,All
     ,None
     ,PLT)

    ,(section "Entertainment")

    (,contrib
     "othello"
     ,(link "othello.plt")
     "Othello (addition to PLT Games application)"
     ,203+
     ,All
     ,None
     (font () "TESQUET S" eacute "bastien"))

    (,contrib
     "hanoi,chess"
     (a ((href "http://kingdom.multimania.com/"))
	"Tower of Hanoi, Chess")
     "Tower of Hanoi animation (source), Chess player (no source)"
     ,101+
     ,All
     ,None
     "Benjamin Cornu")

    (,contrib
     "snakes"
     (a ((href "http://members.xoom.fr/legrandmikey/serpent/enhome.htm"))
	"Snakes")
     "Snake game"
     ,100+
     ,All
     ,None
     (a ((href "http://members.xoom.fr/legrandmikey/"))
	"Mickey Le Grand"))

    (,contrib
     "weather war"
     ,(link "weather-war.zip")
     "Weather War (a classic game)"
     ,103+
     ,All
     ,None
     "Daniel Lyle")

    (,contrib
     "magic.plt"
     (a ((href "http://people.cs.uchicago.edu/~robby/plt/magic.plt"))
	"magic.plt")
     "Search a Magic the Gathering card database"
     ,(seq 200+ ", " '(a ((href "http://people.cs.uchicago.edu/~robby/plt/magic103.plt")) 
			 "103"))
     ,All
     ,None
     ,(seq '(a ((href "http://www.cs.uchicago.edu/~robby/"))
	     "Findler") " and "
           '(a ((href "http://www.cs.rice.edu/~ian/"))
	     "Barland")))

    (,supported-incl
     "games"
     ,(collection "games")
     "Paint-by-number, minesweeper, same, card games"
     ,53+
     ,All
     ,Included
     ,PLT)

    ,(section "Library Collections")

    (,supported-incl
     "regexp,perl"
     (a ((href "http://www.cs.rice.edu/~dorai/pregexp/pregexp.html"))
	"pregexp")
     "Perl-compatible regular expressions"
     ,(included/200 53-103)
     ,All
     ,Included
     (a ((href "http://www.cs.rice.edu/~dorai/"))
	"Dorai Sitaram"))

    (,contrib
     "SLIB"
     (a ((href "http://swissnet.ai.mit.edu/~jaffer/SLIB.html"))
	"SLIB")
     "The Scheme Library"
     ,100++
     ,All
     ,None
     "Aubrey Jaffer et al.")

    ,(section "SRFIs")

    (,supported-incl
     "SRFI"
     ,Schematics
     "Many well-maintained SRFIs"
     ,(included/204 200-203)
     ,All
     ,Included
     ,Schematics)

    ,(section "Textbook Support")

    (,contrib
     "Concrete Abstractions"
     (i (a ((href "http://www.gac.edu/~max/concabs/schemes/drscheme/"))
	   "Concrete Abstractions"))
     (font () "Using DrScheme with "
	   (i (a ((href "http://www.gac.edu/~max/concrete-abstractions.html"))
		 "Concrete Abstractions")))
     ,53+
     ,All
     ,None
     (a ((href "http://www.gac.edu/~max/"))
	"Max Hailperin"))

    (,contrib
     "Structure and Interpretation of Computer Programs"
     (a ((href "http://www-pu.informatik.uni-tuebingen.de/users/sperber/software/picture.plt"))
	"picture.plt")
     (font () "Picture language of "
	   (i (a ((href "http://mitpress.mit.edu/sicp/"))
		 "SICP")))
     ,200+
     ,All
     ,None
     (a ((href "http://www-pu.informatik.uni-tuebingen.de/users/sperber/"))
	"Michael Sperber"))

    ,(section "FFI and C Tools")

    (,contrib
     "Open Cascade, CDL"
     (A ((href "http://dia.uniroma3.it/~scorzell/"))
	"CDL Tool")
     "C++ integeration via CDL (Open Cascade's IDL)"
     ,101+
     ,All
     "C++ compiler"
     "Scorzelli Giorgio")

    (,contrib
     "FFI"
     (a ((href "dyn/"))
        "FFI sketch")
     "A start at calling arbitrary shared-library code from Scheme"
     ,101+
     "Unix"
     "dlopen"
     (a ((href "http://www.cs.cornell.edu/eli/"))
        "Eli Barzilay"))

    (,contrib
     "FFI"
     (a ((href "mpp/"))
        "mpp")
     "A pre-processor for interface code, derived from an ecolisp tool"
     ,101+
     "Cygwin, Unix"
     "C compiler"
     "Gregory Lampshire")

    ,(section "Miscellaneous Utilities")
    
    (,contrib
     "readline"
     ,(link "readline.plt")
     "For using the GNU readline library, especially in the MzScheme REPL"
     ,(seq 205+ ", "
	   `(a ((href "readline-204.plt")) ,200+) ", " 
	   '(a ((href "readline-103.plt")) "103"))
     "Unix, Cygwin?"
     "libreadline.a, C compiler"
     (a ((href "http://www.cs.utah.edu/~mflatt/"))
	"Matthew Flatt"))

    ,(section "Examples and Tests")

    (,contrib
     "Visual Basic"
     ,(link "mzcomvb.zip")
     "Test suite for running MzCOM in Visual Basic 6.0"
     ,102+
     "Windows"
     "Visual Basic"
     "Andre Van Meulebrouck")

    (,contrib
     ""
     ,(link "test-suites.plt")
     "GUI test suite"
     "103"
     ,All
     ,None
     (a ((href "http://www.cs.uchicago.edu/~robby/"))
	"Robby Findler"))

    ))

(define title "PLT Scheme Libraries and Extensions")
(define (ad s)
  `(,s
    (br)))

(define (check-.plt s)
  (if (and (list? s)
	   (= 3 (length s))
	   (string? (caddr s))
	   (regexp-match #rx"[.]plt$" (caddr s)))
      `(td (table ((width "100%")
		   (cellpadding "0")
		   (cellspacing "0"))
		  (tr (td ,s))
		  (tr (td ((align "right"))
			  (i (a ((href "plt-install.html")) "to install..."))))))
      `(td ,s)))

(define (mk-page hd?)
  `(html
    (head
     (title ,title)
     (meta ((name "description")
	    (content "Libraries and extensions for PLT Scheme")))
     (meta ((name "keywords")
	    (content ,(string-append
		       "PLT, Scheme, DrScheme, MzScheme, MrEd"
		       (let loop ([l packages])
			 (cond
			  [(null? l) ""]
			  [(section? (car l)) (loop (cdr l))]
			  [else (let ([rest (loop (cdr l))])
				  (if (cadr (car l))
				      (string-append ", " (cadr (car l)) rest)
				      rest))])))))))
    (body 
     ((bgcolor "white"))

     (h1
      (img ((src "logo.gif")
	    (align "center")
	    (alt "[PLT logo]")))
      ,title)

     "The table below provides a comprehensive list of free libraries and "
     "extensions available for PLT Scheme ("
     ,(plt-page "DrScheme")
     ", "
     ,(plt-page "MzScheme")
     ", and "
     ,(plt-page "MrEd")
     "), "
     "including packages that have been contributed by users, "
     "packages that are downloadable from PLT, "
     "packages that are already included with PLT Scheme distributions, "
     "and packages that are known to be in development."
     
     (p)

     (table (tr (td ((bgcolor "yellow")) (b "See also " (a ((href "http://sourceforge.net/projects/schematics")) "http://sourceforge.net/projects/schematics") "."))))

     (p)

     (table ((border "0"))
	    (tr
	     ,@(if hd?
		   null
		   (map
		    (lambda (x)
		      `(th (b ,x)))
		    '("Package"
		      "Description"
		      "For Versions"
		      "For Platforms"
		      "Other Requirements"
		      "Implementor"))))
	    ,@(map (lambda (r)
		     (if (section? r)
			 `(td (,@head-style
			       (colspan "6"))
			      (b (font ((size "+1"))
				       ,(section-name r)))
			      ,(if hd? '(p) ""))
			 (let ([style (car r)]
			       [keywords (cadr r)]
			       [content (cddr r)])
			   `(tr ,style
				,@(if hd?
				      `((font ()
					      ,(list-ref content 0)
					      " "
					      ,(list-ref content 1)
					      (ul (li "for version: " ,(list-ref content 2))
						  (li "for platform: " ,(list-ref content 3))
						  (li "requirements: " ,(list-ref content 4))
						  (li "implementor: " ,(list-ref content 5)))))
				      (cons
				       (check-.plt (car content))
				       (map
					(lambda (c)
					  `(td ,c))
					(cdr content))))))))
		   packages))
     
     (p)

     (font ((color "red")) (em "Thank you, contributors! "))
     "To contribute software to be included on this page, please send mail to "
     ,(mailto "mflatt@cs.utah.edu")
     ". "

     #|
     "Contributors are encouraged to package software as a "
     (tt ".plt")
     " file (search for "
     (tt "pack")
     " in Help Desk) and to include documentation in the "
     (tt "doc.txt")
     " format (see "
     (tt "doc.txt")
     "in Help Desk), but"
     " contributions will be accepted in other formats."
|#

     (p) (hr) (p)

     (h2 "Help Wanted")
      
     "There are many tools of general interest that we would "
     "like to develop for PLT Scheme, but due to limited resources, "
     "few will be implemented - "
     "unless we can interest other members of the "
     "Scheme community to develop them."

     (p)

     (b
      "Fight the proliferation of "
      (i "ad hoc")
      " languages. "
      "Help to make Scheme useful for real applications. "
      "Be a part of a trailblazing research and development group. "
      "We need your participation.")

     (p)

     "PLT offers timely technical support and advice to any volunteer PLT teammember, "
     "either through direct mail or through the "
     (a ((href "http://www.plt-scheme.org/maillist/"))
	"discussion list")
     "."

     (p)

     "For those who want something to do, here are a few projects the we're especially "
     "interested in supporting:"

     (ul
      (li (b "MzScript")
	 " -"
	 "Implement the Windows Active Scripting interfaces for MzScheme, "
	 "so it can be used in Web pages. "
	 ,(contact "steck@ccs.neu.edu"))
      (li (b "Scsh port")
	  " - "
	  "Port Olin Shivers's shell language to PLT Scheme and extend it to work on multiple platforms. "
	  ,(contact "sk@cs.brown.edu"))
      (li (b "AppleEvents")
	  " - "
	  "Implement an interface for receiving AppleEvents, complementing "
	  "the existing interface for sending them. "
	  ,(contact "mflatt@cs.utah.edu")))

     (p) (hr) (p)

     (table ((width "100%"))
	    (tr
	     (td ((align "right"))
		 (address
		  ,PLT
		  " / scheme@plt-scheme.org")))))))

(with-output-to-file "old-index.html"
  (lambda ()
    (write-xml/content (xexpr->xml (mk-page #f)))
    (newline))
  'truncate/replace)

(with-output-to-file "old-helpdesk.html"
  (lambda ()
    (write-xml/content (xexpr->xml (mk-page #t)))
    (newline))
  'truncate/replace)
