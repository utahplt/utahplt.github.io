#cs
(module lecture26 (lib "slideshow.ss" "slideshow")  
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

  (define (it-split s)
    (map bt (regexp-split " " s)))

  (slide/center
   (vl-append
    line-sep
    (page-para "\"The Future of Software Bugs\"")
    (page-para (it "Computerworld") "(10/27/03) Vol. 31, No. 49, P. 32; Thibodeau, Patrick"))
   (apply
    para
    (* 0.9 client-w)
    "The continuing threat of software bugs stems from a variety of factors,"
    "including software vendors and in-house development teams that rush testing"
    "and sacrifice quality so they can rapidly move products to market;"
    (append
     (it-split
      "academic computer science programs that place more emphasis on development than testing;")
     (list
      "and legislation that absolves developers of the blame for damages users suffer as a result"
      "of faulty products."))))

  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define web-machines
    (comm-machine (lambda (x) (blank)) values values
                  '(plt) (bitmap "ie.bmp")))
  
  (define client-t (t "client"))
  (define server-t (t "server"))
  
  (slide/title
   "Web Server Example"
   'alts~
   (list (list (comm-machine (lambda (x) (blank)) values))
         (list (comm-machine (lambda (x) (blank)) values values))
         (list web-machines)
         (list (ht-append
                (cc-superimpose (ghost client-t) server-t)
                web-machines
                (cc-superimpose client-t (ghost server-t)))))
   'next
   (page-para "Connecting:")
   (table 2
          (list (t "Server:") (code (define l (tcp-listen 4000)))
                (t "Client:") (vl-append
                               line-sep
                               (code (tcp-connect "127.0.0.1" 4000))
                               (page-para* sym:rightarrow (code |#<input-port>| |#<output-port>|)))
                (t "Server:") (vl-append
                               line-sep
                               (code (tcp-accept l))
                               (page-para* sym:rightarrow (code |#<input-port>| |#<output-port>|))))
          lt-superimpose lt-superimpose
          gap-size gap-size))
  
  (slide/center (it "Examples in DrScheme..."))

  (slide/title
   "Web Page Encoding"
   (page-para "A web page is more than plain characters:")
   (frame (inset (vc-append
                  gap-size
                  (scale/improve-new-text (bt "CS 2010") 1.2)
                  (table 3
                         (list
                          (bt "Date") (bt "Topic") (bt "Notes")
                          (t "Nov 3") (t "Java") (colorize (let ([p (t "slides")])
                                                             (place-over p p find-lb (linewidth 2 (hline (pict-width p) 1))))
                                                           BlueColor)
                          (t "Nov 5") (colorize (it "Mid Term 2") RedColor) (blank))
                         lbl-superimpose lbl-superimpose
                         gap-size gap-size))
                 (/ gap-size 2)))
   'next
   (page-para "To encode fonts, color, table layout, links, etc., web servers"
              "and clients communicate using" (dt "XML")
              (blank gap-size 1)
              (scale/improve-new-text (colorize (t "... roughly") BlueColor) 0.7))
   (scale/improve-new-text
    (vl-append
     line-sep
     (tt "<html><p align=\"center\"><font size=\"+2\">CS 2010</font></p>")
     (tt "      <table><tr><td><b>Date</b></td>")
     (tt "                 <td><b>Topic</b></td>")
     (tt "                 <td><b>Notes</b></td></tr>")
     (tt "             <tr><td>Nov 3</td> ...</tr>")
     (tt "             ...</table></html>"))
    0.75))

  (slide/center (it "Examples in DrScheme..."))

  (slide/title/tall
   "Generating XML"
   (page-para "Since XML has an S-expression like structure, and since"
              "we're using Scheme, it makes sense to generate S-expressions"
              "and convert them to XML")
   (blank)
   (scale/improve-new-text
    (code (xexpr->string '(html () "Hello"))
          "should be" "<html>Hello<html>"
          code:blank
          (xexpr->string '(html () "0 < 1"))
          "should be" "<html>0 &lt; 1<html>"
          code:blank
          (xexpr->string '(html () 
                                (font ((size "+2")) 
                                      "Hello")))
          "should be"
          "<html><font size=+2>Hello</font><html>")
    0.8)
   'next
   (blank)
   (page-para "If you're using Java, then you'll generate object trees instead"
              "of S-expressions, but it's the same idea"))

  (slide/center (it "Family tree server in DrScheme..."))
  
  )
