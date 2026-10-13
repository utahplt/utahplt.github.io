3400
((3) 0 () 5 ((q lib "openssl/main.rkt") (q lib "openssl/sha1.rkt") (q lib "openssl/libssl.rkt") (q lib "openssl/md5.rkt") (q lib "openssl/libcrypto.rkt")) () (h ! (equal) ((c def c (c (? . 0) q ssl-accept)) q (2123 . 3)) ((c def c (c (? . 1) q hex-string->bytes)) q (9099 . 3)) ((c def c (c (? . 3) q md5)) q (9166 . 3)) ((c def c (c (? . 0) q ssl-load-verify-root-certificates!)) q (5592 . 7)) ((c def c (c (? . 1) q sha1-bytes)) q (8968 . 3)) ((c def c (c (? . 0) q ssl-load-private-key!)) q (6449 . 10)) ((c def c (c (? . 0) q ssl-connect)) q (85 . 9)) ((c def c (c (? . 0) q ssl-make-server-context)) q (2579 . 4)) ((c def c (c (? . 0) q ssl-set-verify!)) q (8002 . 5)) ((c def c (c (? . 1) q sha1)) q (8911 . 3)) ((c def c (c (? . 0) q ssl-connect/enable-break)) q (481 . 10)) ((c def c (c (? . 2) q libssl)) q (9430 . 2)) ((c def c (c (? . 0) q ssl-set-server-name-identification-callback!)) q (7753 . 5)) ((c def c (c (? . 0) q ssl-seal-context!)) q (6046 . 3)) ((c def c (c (? . 0) q supported-server-protocols)) q (2823 . 3)) ((c def c (c (? . 0) q ssl-make-client-context)) q (968 . 4)) ((c def c (c (? . 0) q ssl-peer-issuer-name)) q (8831 . 3)) ((c def c (c (? . 0) q ssl-listener?)) q (2064 . 3)) ((c def c (c (? . 0) q ssl-dh4096-param-path)) q (7715 . 2)) ((c def c (c (? . 0) q ssl-load-default-verify-sources!)) q (5462 . 3)) ((c def c (c (? . 0) q ssl-addresses)) q (2389 . 4)) ((c def c (c (? . 0) q ssl-try-verify!)) q (8175 . 5)) ((c def c (c (? . 0) q ssl-peer-check-hostname)) q (8642 . 4)) ((c def c (c (? . 0) q supported-client-protocols)) q (1147 . 3)) ((c def c (c (? . 0) q ssl-peer-verified?)) q (8348 . 3)) ((c def c (c (? . 0) q ssl-secure-client-context)) q (905 . 2)) ((c def c (c (? . 0) q ssl-set-verify-hostname!)) q (8416 . 4)) ((c def c (c (? . 4) q openssl-lib-versions)) q (9381 . 2)) ((c def c (c (? . 0) q ssl-peer-certificate-hostnames)) q (8553 . 3)) ((c def c (c (? . 2) q libssl-load-fail-reason)) q (9468 . 2)) ((c def c (c (? . 3) q md5-bytes)) q (9222 . 3)) ((c def c (c (? . 0) q ssl-accept/enable-break)) q (2218 . 3)) ((c def c (c (? . 0) q ssl-load-certificate-chain!)) q (6161 . 6)) ((c def c (c (? . 0) q ports->ssl-ports)) q (3034 . 25)) ((c def c (c (? . 0) q ssl-set-ciphers!)) q (5891 . 4)) ((c def c (c (? . 0) q ssl-peer-subject-name)) q (8750 . 3)) ((c def c (c (? . 0) q ssl-max-server-protocol)) q (2944 . 3)) ((c def c (c (? . 1) q bytes->hex-string)) q (9030 . 3)) ((c def c (c (? . 0) q ssl-max-client-protocol)) q (1333 . 3)) ((c def c (c (? . 0) q ssl-server-context-enable-ecdhe!)) q (7508 . 5)) ((c def c (c (? . 0) q ssl-available?)) q (0 . 2)) ((c def c (c (? . 0) q ssl-port?)) q (2524 . 3)) ((c def c (c (? . 0) q ssl-load-fail-reason)) q (34 . 2)) ((c def c (c (? . 0) q ssl-default-verify-sources)) q (4696 . 13)) ((c def c (c (? . 4) q libcrypto-load-fail-reason)) q (9324 . 2)) ((c def c (c (? . 4) q libcrypto)) q (9283 . 2)) ((c def c (c (? . 0) q ssl-load-verify-source!)) q (4239 . 10)) ((c def c (c (? . 0) q ssl-client-context?)) q (1268 . 3)) ((c def c (c (? . 0) q ssl-abandon-port)) q (2326 . 3)) ((c def c (c (? . 0) q ssl-server-context?)) q (2758 . 3)) ((c def c (c (? . 0) q ssl-listen)) q (1423 . 13)) ((c def c (c (? . 0) q ssl-server-context-enable-dhe!)) q (7280 . 5)) ((c def c (c (? . 0) q ssl-close)) q (1990 . 3)) ((c def c (c (? . 0) q ssl-load-suggested-certificate-authorities!)) q (6896 . 8))))
value
ssl-available? : boolean?
value
ssl-load-fail-reason : (or/c #f string?)
procedure
(ssl-connect  hostname              
              port-no               
             [client-protocol]) -> input-port? output-port?
  hostname : string?
  port-no : (integer-in 1 65535)
  client-protocol : (or/c ssl-client-context?
                          'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                  = 'auto
procedure
(ssl-connect/enable-break  hostname          
                           port-no           
                          [client-protocol]) 
 -> input-port? output-port?
  hostname : string?
  port-no : (integer-in 1 65535)
  client-protocol : (or/c ssl-client-context?
                          'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                  = 'auto
procedure
(ssl-secure-client-context) -> ssl-client-context?
procedure
(ssl-make-client-context [protocol]) -> ssl-client-context?
  protocol : (or/c 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
           = 'auto
procedure
(supported-client-protocols)
 -> (listof (or/c 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12))
procedure
(ssl-client-context? v) -> boolean?
  v : any/c
procedure
(ssl-max-client-protocol)
 -> (or/c 'sslv2 sslv3 'tls 'tls11 'tls12 #f)
procedure
(ssl-listen  port-no               
            [queue-k               
             reuse?                
             hostname-or-#f        
             server-protocol]) -> ssl-listener?
  port-no : (integer-in 1 65535)
  queue-k : exact-nonnegative-integer? = 5
  reuse? : any/c = #f
  hostname-or-#f : (or/c string? #f) = #f
  server-protocol : (or/c ssl-server-context?
                          'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                  = 'auto
procedure
(ssl-close listener) -> void?
  listener : ssl-listener?
procedure
(ssl-listener? v) -> boolean?
  v : any/c
procedure
(ssl-accept listener) -> input-port? output-port?
  listener : ssl-listener?
procedure
(ssl-accept/enable-break listener) -> input-port? output-port?
  listener : ssl-listener?
procedure
(ssl-abandon-port p) -> void?
  p : ssl-port?
procedure
(ssl-addresses p [port-numbers?]) -> void?
  p : (or/c ssl-port? ssl-listener?)
  port-numbers? : any/c = #f
procedure
(ssl-port? v) -> boolean?
  v : any/c
procedure
(ssl-make-server-context [protocol]) -> ssl-server-context?
  protocol : (or/c 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
           = 'auto
procedure
(ssl-server-context? v) -> boolean?
  v : any/c
procedure
(supported-server-protocols)
 -> (listof (or/c 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12))
procedure
(ssl-max-server-protocol)
 -> (or/c 'sslv2 sslv3 'tls 'tls11 'tls12 #f)
procedure
(ports->ssl-ports  input-port                              
                   output-port                             
                  [#:mode mode                             
                   #:context context                       
                   #:encrypt protocol                      
                   #:close-original? close-original?       
                   #:shutdown-on-close? shutdown-on-close? 
                   #:error/ssl error                       
                   #:hostname hostname])                   
 -> input-port? output-port?
  input-port : input-port?
  output-port : output-port?
  mode : symbol? = 'accept
  context : (or/c ssl-client-context? ssl-server-context?)
          = ((if (eq? mode 'accept)
                 ssl-make-server-context
                 ssl-make-client-context)
             protocol)
  protocol : (or/c 'auto 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
           = 'auto
  close-original? : boolean? = #f
  shutdown-on-close? : boolean? = #f
  error : procedure? = error
  hostname : (or/c string? #f) = #f
procedure
(ssl-load-verify-source!  context           
                          src               
                         [#:try? try?]) -> void?
  context : (or/c ssl-client-context? ssl-server-context?)
  src : (or/c path-string?
              (list/c 'directory path-string?)
              (list/c 'win32-store string?)
              (list/c 'macosx-keychain path-string?))
  try? : any/c = #f
parameter
(ssl-default-verify-sources)
 -> (let ([source/c (or/c path-string?
                          (list/c 'directory path-string?)
                          (list/c 'win32-store string?)
                          (list/c 'macosx-keychain path-string?))])
      (listof source/c))
(ssl-default-verify-sources srcs) -> void?
  srcs : (let ([source/c (or/c path-string?
                               (list/c 'directory path-string?)
                               (list/c 'win32-store string?)
                               (list/c 'macosx-keychain path-string?))])
           (listof source/c))
procedure
(ssl-load-default-verify-sources! context) -> void?
  context : (or/c ssl-client-context? ssl-server-context?)
procedure
(ssl-load-verify-root-certificates! context-or-listener 
                                    pathname)           
 -> void?
  context-or-listener : (or/c ssl-client-conntext? ssl-server-context?
                              ssl-listener?)
  pathname : path-string?
procedure
(ssl-set-ciphers! context cipher-spec) -> void?
  context : (or/c ssl-client-context? ssl-server-context?)
  cipher-spec : string?
procedure
(ssl-seal-context! context) -> void?
  context : (or/c ssl-client-context? ssl-server-context?)
procedure
(ssl-load-certificate-chain! context-or-listener     
                             pathname)           -> void?
  context-or-listener : (or/c ssl-client-context? ssl-server-context?
                              ssl-listener?)
  pathname : path-string?
procedure
(ssl-load-private-key!  context-or-listener     
                        pathname                
                       [rsa?                    
                        asn1?])             -> void?
  context-or-listener : (or/c ssl-client-context? ssl-server-context?
                              ssl-listener?)
  pathname : path-string?
  rsa? : boolean? = #t
  asn1? : boolean? = #f
procedure
(ssl-load-suggested-certificate-authorities!                     
                                             context-or-listener 
                                             pathname)           
 -> void?
  context-or-listener : (or/c ssl-client-context? ssl-server-context?
                              ssl-listener?)
  pathname : path-string?
procedure
(ssl-server-context-enable-dhe!  context             
                                [dh-param-path]) -> void?
  context : ssl-server-context?
  dh-param-path : path-string? = ssl-dh4096-param-path
procedure
(ssl-server-context-enable-ecdhe!  context          
                                  [curve-name]) -> void?
  context : ssl-server-context?
  curve-name : symbol? = 'secp521r1
value
ssl-dh4096-param-path : path?
procedure
(ssl-set-server-name-identification-callback! context       
                                              callback) -> void?
  context : ssl-server-context?
  callback : (string? . -> . (or/c ssl-server-context? #f))
procedure
(ssl-set-verify! clp on?) -> void?
  clp : (or/c ssl-client-context? ssl-server-context?
              ssl-listener? ssl-port?)
  on? : any/c
procedure
(ssl-try-verify! clp on?) -> void?
  clp : (or/c ssl-client-context? ssl-server-context?
              ssl-listener? ssl-port?)
  on? : any/c
procedure
(ssl-peer-verified? p) -> boolean?
  p : ssl-port?
procedure
(ssl-set-verify-hostname! ctx on?) -> void?
  ctx : (or/c ssl-client-context? ssl-server-context?)
  on? : any/c
procedure
(ssl-peer-certificate-hostnames p) -> (listof string?)
  p : ssl-port?
procedure
(ssl-peer-check-hostname p hostname) -> boolean?
  p : ssl-port?
  hostname : string?
procedure
(ssl-peer-subject-name p) -> (or/c bytes? #f)
  p : ssl-port?
procedure
(ssl-peer-issuer-name p) -> (or/c bytes? #f)
  p : ssl-port?
procedure
(sha1 in) -> string?
  in : input-port?
procedure
(sha1-bytes in) -> bytes?
  in : input-port?
procedure
(bytes->hex-string bstr) -> string?
  bstr : bytes?
procedure
(hex-string->bytes str) -> bytes?
  str : string?
procedure
(md5 in) -> string?
  in : input-port?
procedure
(md5-bytes in) -> bytes?
  in : input-port?
value
libcrypto : (or/c #f ffi-lib?)
value
libcrypto-load-fail-reason : (or/c #f string?)
value
openssl-lib-versions : (listof string?)
value
libssl : (or/c #f ffi-lib?)
value
libssl-load-fail-reason : (or/c #f string?)
