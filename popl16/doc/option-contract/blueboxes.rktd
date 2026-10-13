485
((3) 0 () 1 ((q lib "racket/contract/option.rkt")) () (h ! (equal) ((c def c (c (? . 0) q waive-option)) q (755 . 3)) ((c def c (c (? . 0) q tweak-option)) q (810 . 3)) ((c def c (c (? . 0) q invariant/c)) q (993 . 11)) ((c def c (c (? . 0) q option/c)) q (0 . 15)) ((c def c (c (? . 0) q exercise-option)) q (666 . 3)) ((c def c (c (? . 0) q has-option?)) q (865 . 3)) ((c def c (c (? . 0) q transfer/c)) q (724 . 2)) ((c def c (c (? . 0) q has-option-with-contract?)) q (922 . 3))))
procedure
(option/c  c                         
          [#:with-contract with      
           #:tester tester           
           #:invariant invariant     
           #:immutable immutable     
           #:flat? flat?             
           #:struct struct-id])  -> contract?
  c : contract?
  with : boolean? = #f
  tester : (or/c (-> any boolean?) 'dont-care) = 'dont-care
  invariant : (or/c (-> any boolean?) 'dont-care) = 'dont-care
  immutable : (or/c #t #f 'dont-care) = 'dont-care
  flat? : boolean? = #f
  struct-id : (or/c identifier? 'none) = 'none
procedure
(exercise-option x) -> any/c
  x : any/c
value
transfer/c : contract?
procedure
(waive-option x) -> any/c
  x : any/c
procedure
(tweak-option x) -> any/c
  x : any/c
procedure
(has-option? v) -> boolean?
  v : any/c
procedure
(has-option-with-contract? v) -> boolean?
  v : any/c
procedure
(invariant/c  c                         
              invariant                 
             [#:immutable immutable     
              #:flat? flat?             
              #:struct struct-id])  -> contract?
  c : contract?
  invariant : (-> any boolean?)
  immutable : (or/c #t #f 'dont-care) = 'dont-care
  flat? : boolean? = #f
  struct-id : (or/c identifier? 'none) = 'none
