480
((3) 0 () 2 ((q lib "pict/snip.rkt") (c (? . 0) q pict-snip%)) () (h ! (equal) ((c def c (c (? . 0) q reader)) q (2028 . 2)) ((c def c (c (? . 0) q snipclass)) q (1985 . 2)) ((c meth c (c (? . 1) q draw)) q (989 . 23)) ((c def c (? . 1)) q (0 . 3)) ((c meth c (c (? . 1) q get-extent)) q (189 . 17)) ((c meth c (c (? . 1) q write)) q (1837 . 3)) ((c constructor c (? . 1)) q (51 . 3)) ((c meth c (c (? . 1) q copy)) q (1925 . 2)) ((c meth c (c (? . 1) q get-pict)) q (141 . 2))))
class
pict-snip% : class?
  superclass: snip%
constructor
(new pict-snip% [pict pict]) -> (is-a?/c pict-snip%)
  pict : pict?
method
(send a-pict-snip get-pict) -> pict?
method
(send a-pict-snip get-extent dc          
                             x           
                             y           
                             w           
                             h           
                             descent     
                             lspace      
                             rspace) -> void?
  dc : (is-a?/c dc<%>)
  x : real?
  y : real?
  w : (or/c (box/c (and/c real? (not/c negative?))) #f)
  h : (or/c (box/c (and/c real? (not/c negative?))) #f)
  descent : (or/c (box/c (and/c real? (not/c negative?))) #f)
  lspace : (or/c (box/c (and/c real? (not/c negative?))) #f)
  rspace : (or/c (box/c (and/c real? (not/c negative?))) #f)
method
(send a-pict-snip draw dc              
                       x               
                       y               
                       left            
                       top             
                       right           
                       bottom          
                       dx              
                       dy              
                       draw-caret) -> void?
  dc : (is-a?/c dc<%>)
  x : real?
  y : real?
  left : real?
  top : real?
  right : real?
  bottom : real?
  dx : real?
  dy : real?
  draw-caret : (or/c 'no-caret 'show-inactive-caret 'show-caret
                     (cons/c exact-nonnegative-integer?
                             exact-nonnegative-integer?))
method
(send a-pict-snip write f) -> void?
  f : (is-a?/c editor-stream-out%)
method
(send a-pict-snip copy) -> (is-a?/c pict-snip%)
value
snipclass : (is-a?/c snip-class%)
value
reader : (is-a?/c snip-reader<%>)
