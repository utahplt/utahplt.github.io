(module gcpict (lib "slideshow.ss" "texpict")

  (require "colors.ss"
	   "utils.ss"
	   "alg.ss")
  
  (provide make-gc-picture)

  (define base-size (* 1 font-size))
  
  (define recsize (* base-size 2.5))
  (define spacewidth (* recsize 4))
  (define spaceheight (* recsize 6))
  
  (define slot1 (blank recsize base-size))
  (define slot2 (blank recsize base-size))
  
  (define (spaceframe)
    (color-frame (blank spacewidth spaceheight)
                 "blue" 2))
  
  (define (places . l)
    (let loop ([l l])
      (if (null? l)
          null
          (cons `(place ,(* (car l) spacewidth)
                        ,(- (- spaceheight (* (cadr l) spaceheight))
                            recsize)
                        ,(caddr l))
                (loop (cdddr l))))))
  
  (define (dot color)
    (inset (colorize (disk (- font-size 4)) color) 2 2))
  
  (define hi-arrow (colorize (arrowhead (/ font-size 2) (- pi)) red))
  
  (define (make-gc-picture desc grays blacks hilite)
    (define (recframe name rc n)
      (let ([b (color-frame
                (inset
                 (if (= n 1)
                     slot1
                     (vc-append slot1 slot2))
                 0 (* base-size 0.5) 0 0)
                (cond
                  [(or (and (eq? name 'b)
                            (memq 'bcopy desc))
                       (and (memq name '(a e))
                            (memq 'aecopy desc))
                       (and (eq? name 'g)
                            (memq 'gcopy desc)))
                   "red"]
                  [else "green"])
                2)])
        (cond
          [(and rc (memq 'rc desc))
           (lt-superimpose
            b
            (tt rc))]
          [(memq name grays)
           (let ([d (dot "gray")])
             (lt-superimpose 
              b
              (if (eq? name hilite)
                  (hc-append d hi-arrow)
                  d)))]
          [(memq name blacks)
           (lt-superimpose b (dot "black"))]
          [(eq? name hilite)
           (inset (ht-append b hi-arrow) 0 0 (- (pict-width hi-arrow)) 0)]
          [else b])))
    (let ([reg1 (recframe 'reg1 #f 1)]
          [reg2 (recframe 'reg2 #f 1)]
          [orig-space (spaceframe)]
          [a (recframe 'a "1" 2)]
          [b (recframe 'b "1"2)]
          [c (let ([p (recframe 'c (if (memq 'moved desc) "0" "1") 2)])
               (if (memq 'freed desc)
                   (ghost p)
                   p))]
          [d (let ([p (recframe 'd (if (memq 'cycle desc)
                                       (if (memq 'gone desc)
                                           "1"
                                           "2")
                                       (if (memq 'gone desc)
                                           "0"
                                           "1")) 2)])
               (if (or (memq 'cyclegone desc)
                       (memq 'dfreed desc))
                   (ghost p)
                   p))]
          [e (recframe 'e (if (memq 'moved desc) 
                              (if (memq 'freed desc) 
                                  "2"
                                  "3")
                              "2") 2)]
          [f (let ([p (recframe 'f (if (memq 'dfreed desc) 
                                       "0" 
                                       "1") 2)])
               (if (or (memq 'cyclegone desc)
                       (memq 'ffreed desc))
                   (ghost p)
                   p))]
          [g (recframe 'g "1" 2)]
          [other-space (spaceframe)]
          [b2 (let ([p (recframe 'b2 #f 2)])
                (if (memq 'bcopy desc)
                    p
                    (ghost p)))]
          [e2 (let ([p (recframe 'e2 #f 2)])
                (if (memq 'aecopy desc)
                    p
                    (ghost p)))]
          [a2 (let ([p (recframe 'a2 #f 2)])
                (if (memq 'aecopy desc)
                    p
                    (ghost p)))]
          [g2 (let ([p (recframe 'g2 #f 2)])
                (if (memq 'gcopy desc)
                    p
                    (ghost p)))])
      (let ([orig-space+
             (cons-picture
              orig-space
              (places
               0.6 0.05 a
               0.15 0.1 b
               0.50 0.3 c
               0.05 0.4 d
               0.55 0.55 e
               0.1 0.75 f
               0.7 0.8 g))]
            [other-space
             (cons-picture
              other-space
              (places
               0.2 0.05 b2
               0.1 0.30 a2
               0.5 0.30 e2
               0.1 0.55 g2))]
            [add-top
             (lambda (p)
               (vl-append
                (/ recsize 2)
                (hc-append recsize reg1 reg2)
                p))])
        (let ([all
               (if (memq 2 desc)
                   (hb-append
                    recsize
                    (add-top
                     (if (memq '2only desc)
                         (cc-superimpose
                          orig-space
                          (ghost orig-space+))
                         orig-space+))
                    other-space)
                   (add-top orig-space+))])
          (let ([oc (lambda (src slot dest find-dest)
                      (set! all
                            (add-arrow-line
                             (/ font-size 2) all
                             (list src slot) find-cc
                             dest (or find-dest find-ct)
                             2 orange)))])
            (oc reg2 slot1 (if (memq 'bcopy desc) b2 b) #f)
            (when (memq 'bcopy desc)
              (unless (memq '2only desc)
                (oc b slot1 b2 find-lc)))
            (when (memq 'aecopy desc)
              (unless (memq '2only desc)
                (oc a slot1 a2 find-lc)
                (oc e slot1 e2 find-lc)))
            (if (memq 'aecopy desc)
                (begin
                  (oc b2 slot1 a2 #f)
                  (oc b2 slot2 e2 #f))
                (let ([b (if (memq 'bcopy desc) b2 b)])
                  (oc b slot1 a (if (memq 'bcopy desc) find-rt find-lc))
                  (oc b slot2 (if (memq 'moved desc)
                                  e
                                  c) 
                      (if (memq 'bcopy desc) find-rc find-lt))))
            (unless (memq 'freed desc)
              (unless (memq '2only desc)
                (oc c slot2 e #f)))
            (when (memq 'gcopy desc)
              (unless (memq '2only desc)
                (oc g slot1 g2 find-lb)))
            (oc (if (memq 'aecopy desc) e2 e) 
                slot2 
                (if (memq 'gcopy desc) g2 g)
                #f)
            (unless (memq 'gone desc)
              (oc reg1 slot1 d find-lt))
            (unless (memq 'cyclegone desc)
              (unless (memq 'dfreed desc)
                (unless (memq '2only desc)
                  (oc d slot2 f #f)))
              (when (memq 'cycle desc)
                (unless (memq '2only desc)
                  (oc f slot2 d find-rb))))
            (oc (if (memq 'gcopy desc) g2 g) 
                slot1 
                (if (memq 'gupdate desc) e2 e) 
                find-cb)
            
            all))))))
