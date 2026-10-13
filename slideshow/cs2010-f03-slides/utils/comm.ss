#cs
(module comm (lib "slideshow.ss" "slideshow")
  (require (lib "math.ss")
	   (lib "etc.ss")
	   "colors.ss")

  (provide comm-machine)

  (define (nw w h angle)
    (colorize
     (linewidth
      3
      (hb-append (vline 1 h)
		 (inset
		  (cc-superimpose
		   (hline w 1)
		   (colorize (arrowhead gap-size angle) GreenColor))
		  0 0 0 (* gap-size -1/2))
		 (vline 1 h)))
     BlueColor))

  (define comm-machine
    (opt-lambda (vonly-file vafter-network [vafter-network2 (lambda (x) (blank))]
			    [m-style-1 null] [m-style-2 null])
      (vc-append
       (inset
	(hc-append (* 3 gap-size) 
		   (desktop-machine 1.5 m-style-1) 
		   (desktop-machine 1.5 m-style-2))
	0 0 0 (- 30))
       (ct-superimpose
	(vonly-file
	 (vc-append
	  gap-size
	  (blank)
	  (colorize
	   (hc-append (* 3 gap-size)
		      (arrow (* 2 gap-size) (* pi 7/4))
		      (arrow (* 2 gap-size) (* pi 1/4)))
	   GreenColor)
	  (file-icon (* 3 gap-size) (* 4 gap-size) #t)))
	(vafter-network
	 (nw (* gap-size 10) (* gap-size 4) 0))
	(vafter-network2
	 (nw (* gap-size 12) (* gap-size 6) pi)))))))
