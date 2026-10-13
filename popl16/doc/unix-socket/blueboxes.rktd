237
((3) 0 () 1 ((q lib "racket/unix-socket.rkt")) () (h ! (equal) ((c def c (c (? . 0) q unix-socket-available?)) q (0 . 2)) ((c def c (c (? . 0) q unix-socket-connect)) q (42 . 3)) ((c def c (c (? . 0) q unix-socket-path?)) q (156 . 3))))
value
unix-socket-available? : boolean?
procedure
(unix-socket-connect socket-path) -> input-port? output-port?
  socket-path : unix-socket-path?
procedure
(unix-socket-path? v) -> boolean?
  v : any/c
