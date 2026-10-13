15599
((3) 0 () 40 ((q lib "web-server/private/dispatch-server-sig.rkt") (q lib "web-server/configuration/configuration-table-structs.rkt") (q 18356 . 18) (q lib "web-server/private/connection-manager.rkt") (q 22570 . 8) (q 20035 . 16) (q lib "web-server/dispatchers/dispatch-servlets.rkt") (q 17097 . 12) (q 17620 . 8) (q lib "web-server/private/util.rkt") (q lib "web-server/configuration/responders.rkt") (q lib "web-server/web-config-sig.rkt") (q 17880 . 16) (q lib "web-server/private/timer.rkt") (q lib "web-server/web-server.rkt") (q 19129 . 16) (q lib "web-server/private/servlet.rkt") (q 6075 . 9) (q lib "web-server/dispatchers/dispatch.rkt") (q lib "web-server/dispatchers/dispatch-passwords.rkt") (q lib "web-server/configuration/namespace.rkt") (q 21985 . 7) (q 1398 . 3) (q 19654 . 12) (q lib "web-server/dispatchers/dispatch-log.rkt") (q lib "web-server/private/cache-table.rkt") (q lib "web-server/servlet/setup.rkt") (q lib "web-server/configuration/configuration-table.rkt") (q lib "web-server/web-server-unit.rkt") (q lib "web-server/private/mime-types.rkt") (q lib "web-server/private/mod-map.rkt") (q lib "web-server/dispatchers/filesystem-map.rkt") (q lib "web-server/dispatchers/dispatch-stat.rkt") (q lib "web-server/web-server-sig.rkt") (q lib "web-server/private/url-param.rkt") (q lib "web-server/private/dispatch-server-unit.rkt") (q lib "web-server/web-config-unit.rkt") (q lib "web-server/lang/serial-lambda.rkt") (q lib "web-server/servlet-dispatch.rkt") (q lib "web-server/private/gzip.rkt")) () (h ! (equal) ((c def c (c (? . 10) q servlet-loading-responder)) q (20886 . 4)) ((q def ((lib "web-server/dispatchers/dispatch-pathprocedure.rkt") make)) q (2184 . 4)) ((c def c (c (? . 6) q make)) q (4074 . 10)) ((c form c (c (? . 0) q dispatch-server-config^)) q (399 . 2)) ((c sig-val c (c (? . 0) q dispatch-server-connect^ port->real-ports)) q (284 . 4)) ((c def c (c (? . 1) q responders-collect-garbage)) c (? . 2)) ((c def c (c (? . 1) q struct:responders)) c (? . 2)) ((c def c (c (? . 1) q messages-passwords-refreshed)) c (? . 15)) ((c def c (c (? . 1) q paths-host-base)) c (? . 5)) ((c def c (c (? . 1) q responders-servlet-loading)) c (? . 2)) ((c sig-val c (c (? . 0) q dispatch-server-config^ listen-ip)) q (482 . 2)) ((c def c (c (? . 1) q struct:configuration-table)) c (? . 7)) ((c def c (c (? . 16) q struct:servlet)) c (? . 17)) ((c def c (c (? . 3) q connection-custodian)) c (? . 4)) ((c def c (c (? . 3) q make-connection)) c (? . 4)) ((c def c (c (? . 13) q start-timer)) q (22237 . 5)) ((c def c (c (? . 1) q paths-servlet)) c (? . 5)) ((c def c (c (? . 3) q new-connection)) q (22944 . 13)) ((c def c (c (? . 6) q make-cached-url->servlet)) q (3874 . 6)) ((c def c (c (? . 14) q serve/web-config@)) q (10752 . 11)) ((c sig-val c (c (? . 0) q dispatch-server^ serve)) q (41 . 3)) ((c def c (c (? . 1) q configuration-table-default-host)) c (? . 7)) ((c def c (c (? . 1) q configuration-table-max-waiting)) c (? . 7)) ((c def c (c (? . 9) q path-without-base)) q (24945 . 4)) ((c def c (c (? . 29) q make-path->mime-type)) q (24028 . 3)) ((c def c (c (? . 32) q make-gc-thread)) q (6381 . 3)) ((c def c (c (? . 1) q host-table-timeouts)) c (? . 8)) ((c def c (c (? . 1) q host-indices)) c (? . 12)) ((c def c (c (? . 1) q struct:host-table)) c (? . 8)) ((c def c (c (? . 9) q exn->string)) q (25338 . 3)) ((c def c (c (? . 1) q responders-servlet)) c (? . 2)) ((c sig-val c (c (? . 11) q web-config^ initial-connection-timeout)) q (14636 . 2)) ((c def c (c (? . 10) q gen-authentication-responder)) q (21478 . 4)) ((c sig-val c (c (? . 11) q web-config^ make-servlet-namespace)) q (14755 . 2)) ((c def c (c (? . 18) q dispatcher/c)) q (1285 . 2)) ((c def c (c (? . 10) q gen-collect-garbage-responder)) q (21830 . 3)) ((c def c (c (? . 1) q timeouts?)) c (? . 23)) ((c def c (c (? . 1) q host-timeouts)) c (? . 12)) ((c def c (c (? . 1) q struct:host)) c (? . 12)) ((c def c (c (? . 36) q configuration-table->web-config@)) q (14813 . 12)) ((c sig-val c (c (? . 0) q dispatch-server-config^ read-request)) q (622 . 5)) ((c def c (c (? . 13) q start-timer-manager)) q (22185 . 2)) ((c def c (c (? . 1) q responders-file-not-found)) c (? . 2)) ((c def c (c (? . 9) q bytes-ci=?)) q (24563 . 4)) ((c def c (c (? . 20) q make-servlet-namespace/c)) q (5846 . 2)) ((c def c (c (? . 14) q serve/ips+ports)) q (9373 . 22)) ((c def c (c (? . 28) q web-server-with-connect@)) q (14281 . 4)) ((c def c (c (? . 1) q make-paths)) c (? . 5)) ((c def c (c (? . 1) q struct:messages)) c (? . 15)) ((c def c (c (? . 1) q messages-servlet)) c (? . 15)) ((c def c (c (? . 1) q configuration-table-virtual-hosts)) c (? . 7)) ((c def c (c (? . 1) q host)) c (? . 12)) ((c def c (c (? . 16) q servlet-namespace)) c (? . 17)) ((c def c (c (? . 10) q gen-passwords-refreshed)) q (21389 . 3)) ((c def c (c (? . 34) q insert-param)) q (24257 . 5)) ((c def c (c (? . 26) q path->servlet/c)) q (5351 . 2)) ((q def ((lib "web-server/dispatchers/dispatch-filter.rkt") make)) q (2086 . 4)) ((c def c (c (? . 1) q host-table)) c (? . 8)) ((q def ((lib "web-server/dispatchers/dispatch-host.rkt") make)) q (3338 . 3)) ((c def c (c (? . 19) q make-basic-denied?/path)) q (3119 . 3)) ((c sig-val c (c (? . 0) q dispatch-server^ serve-ports)) q (146 . 4)) ((c def c (c (? . 13) q timer?)) c (? . 21)) ((q def ((lib "web-server/dispatchers/dispatch-lift.rkt") make)) q (2002 . 3)) ((c def c (c (? . 1) q configuration-table-initial-connection-timeout)) c (? . 7)) ((c def c (c (? . 1) q messages-file-not-found)) c (? . 15)) ((c def c (c (? . 9) q write/bytes)) q (25478 . 3)) ((c def c (c (? . 18) q make-exn:dispatcher)) c (? . 22)) ((c def c (c (? . 1) q make-messages)) c (? . 15)) ((c def c (c (? . 19) q password-file->authorized?)) q (3217 . 4)) ((c def c (c (? . 14) q serve/ports)) q (7987 . 23)) ((c def c (c (? . 27) q configuration-table-sexpr?)) q (16588 . 2)) ((c def c (c (? . 16) q servlet?)) c (? . 17)) ((c def c (c (? . 16) q set-servlet-directory!)) c (? . 17)) ((c def c (c (? . 20) q make-make-servlet-namespace)) q (5891 . 4)) ((c def c (c (? . 10) q file-response)) q (20596 . 9)) ((c def c (c (? . 25) q cache-table?)) q (23885 . 3)) ((c def c (c (? . 1) q host-log-path)) c (? . 12)) ((c def c (c (? . 16) q make-servlet)) c (? . 17)) ((c def c (c (? . 3) q connection?)) c (? . 4)) ((c def c (c (? . 10) q gen-protocol-responder)) q (21609 . 3)) ((c def c (c (? . 30) q decompress-serial)) q (24193 . 3)) ((c form c (c (? . 33) q web-server^)) q (14122 . 2)) ((c def c (c (? . 24) q format-req/c)) q (2295 . 2)) ((c def c (c (? . 13) q timer-tm)) c (? . 21)) ((c def c (c (? . 1) q paths-htdocs)) c (? . 5)) ((c def c (c (? . 3) q kill-connection!)) q (23329 . 3)) ((c def c (c (? . 18) q exn:dispatcher?)) c (? . 22)) ((c def c (c (? . 9) q directory-part)) q (25060 . 3)) ((c def c (c (? . 1) q configuration-table)) c (? . 7)) ((c def c (c (? . 16) q servlet-handler)) c (? . 17)) ((c def c (c (? . 1) q timeouts-default-servlet)) c (? . 23)) ((c def c (c (? . 13) q reset-timer!)) q (22358 . 4)) ((c def c (c (? . 18) q exn:dispatcher)) c (? . 22)) ((c form c (c (? . 0) q dispatch-server^)) q (0 . 2)) ((c def c (c (? . 1) q paths-conf)) c (? . 5)) ((c sig-val c (c (? . 0) q dispatch-server-config^ dispatch)) q (815 . 2)) ((c def c (c (? . 31) q url->path/c)) q (1526 . 2)) ((c def c (c (? . 3) q connection-o-port)) c (? . 4)) ((c sig-val c (c (? . 0) q dispatch-server-config^ max-waiting)) q (527 . 2)) ((q def ((lib "web-server/dispatchers/dispatch-sequencer.rkt") make)) q (1841 . 3)) ((c sig-val c (c (? . 11) q web-config^ virtual-hosts)) q (14587 . 2)) ((c def c (c (? . 1) q messages)) c (? . 15)) ((c def c (c (? . 24) q paren-format)) q (2328 . 2)) ((c def c (c (? . 3) q connection)) c (? . 4)) ((c def c (c (? . 25) q cache-table-clear!)) q (23815 . 3)) ((c def c (c (? . 10) q gen-servlets-refreshed)) q (21301 . 3)) ((c def c (c (? . 26) q make-v2.servlet)) q (4769 . 5)) ((c def c (c (? . 16) q servlet-custodian)) c (? . 17)) ((c def c (c (? . 1) q timeouts-file-per-byte)) c (? . 23)) ((c def c (c (? . 25) q make-cache-table)) q (23647 . 2)) ((c def c (c (? . 14) q do-not-return)) q (11716 . 2)) ((c def c (c (? . 27) q configuration-table->sexpr)) q (16762 . 4)) ((c def c (c (? . 28) q web-server@)) q (14406 . 3)) ((q def ((lib "web-server/dispatchers/dispatch-files.rkt") make)) q (3450 . 8)) ((c def c (c (? . 1) q host-table-paths)) c (? . 8)) ((c def c (c (? . 1) q timeouts-servlet-connection)) c (? . 23)) ((c def c (c (? . 25) q cache-table-lookup!)) q (23694 . 5)) ((c def c (c (? . 1) q host-paths)) c (? . 12)) ((c def c (c (? . 29) q read-mime-types)) q (23943 . 3)) ((c def c (c (? . 18) q dispatcher-interface-version/c)) q (1318 . 3)) ((c def c (c (? . 1) q paths-mime-types)) c (? . 5)) ((c sig-val c (c (? . 0) q dispatch-server-config^ initial-connection-timeout)) q (576 . 2)) ((c def c (c (? . 1) q host-table-messages)) c (? . 8)) ((c def c (c (? . 13) q make-timer)) c (? . 21)) ((c def c (c (? . 24) q extended-format)) q (2364 . 2)) ((c sig-val c (c (? . 11) q web-config^ listen-ip)) q (14710 . 2)) ((c def c (c (? . 1) q responders)) c (? . 2)) ((c def c (c (? . 1) q configuration-table?)) c (? . 7)) ((c def c (c (? . 1) q make-timeouts)) c (? . 23)) ((c def c (c (? . 30) q compress-serial)) q (24133 . 3)) ((c form c (c (? . 11) q web-config^)) q (14502 . 2)) ((c def c (c (? . 16) q set-servlet-custodian!)) c (? . 17)) ((c def c (c (? . 10) q gen-servlet-not-found)) q (20983 . 3)) ((c def c (c (? . 13) q timer)) c (? . 21)) ((c sig-val c (c (? . 11) q web-config^ port)) q (14682 . 2)) ((c def c (c (? . 10) q gen-file-not-found-responder)) q (21714 . 4)) ((c def c (c (? . 1) q struct:timeouts)) c (? . 23)) ((c def c (c (? . 1) q messages-protocol)) c (? . 15)) ((c def c (c (? . 27) q read-configuration-table)) q (16878 . 3)) ((c def c (c (? . 1) q host?)) c (? . 12)) ((c def c (c (? . 1) q paths?)) c (? . 5)) ((c def c (c (? . 27) q write-configuration-table)) q (16973 . 4)) ((c def c (c (? . 10) q gen-servlet-responder)) q (21182 . 4)) ((c def c (c (? . 19) q authorized?/c)) q (3085 . 2)) ((c def c (c (? . 1) q messages-collect-garbage)) c (? . 15)) ((c def c (c (? . 31) q make-url->path)) q (1558 . 3)) ((c def c (c (? . 1) q make-responders)) c (? . 2)) ((c def c (c (? . 1) q responders-authentication)) c (? . 2)) ((c def c (c (? . 32) q make)) q (6449 . 2)) ((c sig-val c (c (? . 33) q web-server^ serve-ports)) q (14192 . 4)) ((c def c (c (? . 26) q make-default-path->servlet)) q (5387 . 8)) ((c def c (c (? . 14) q raw:dispatch-server-connect@)) q (11396 . 3)) ((c def c (c (? . 35) q dispatch-server@)) q (1096 . 4)) ((c def c (c (? . 1) q timeouts)) c (? . 23)) ((c def c (c (? . 3) q struct:connection)) c (? . 4)) ((c def c (c (? . 9) q url-path->string)) q (24775 . 3)) ((c def c (c (? . 9) q build-path-unless-absolute)) q (25124 . 4)) ((c def c (c (? . 1) q make-host-table)) c (? . 8)) ((c def c (c (? . 26) q make-v1.servlet)) q (4593 . 5)) ((c def c (c (? . 1) q host-table?)) c (? . 8)) ((c def c (c (? . 1) q struct:paths)) c (? . 5)) ((c sig-val c (c (? . 33) q web-server^ serve)) q (14158 . 2)) ((c def c (c (? . 1) q messages-authentication)) c (? . 15)) ((c def c (c (? . 1) q configuration-table-port)) c (? . 7)) ((c def c (c (? . 10) q servlet-error-responder)) q (21087 . 4)) ((c def c (c (? . 13) q timer-expire-seconds)) c (? . 21)) ((c def c (c (? . 1) q make-host)) c (? . 12)) ((c def c (c (? . 1) q host-table-indices)) c (? . 8)) ((c def c (c (? . 3) q connection-timer)) c (? . 4)) ((c def c (c (? . 1) q timeouts-password)) c (? . 23)) ((c def c (c (? . 38) q serve/launch/wait)) q (13100 . 20)) ((c def c (c (? . 3) q start-connection-manager)) q (22882 . 2)) ((c def c (c (? . 16) q servlet)) c (? . 17)) ((c def c (c (? . 1) q responders-protocol)) c (? . 2)) ((c def c (c (? . 27) q default-configuration-table-path)) q (16539 . 2)) ((c def c (c (? . 24) q log-format->format)) q (2481 . 3)) ((c def c (c (? . 3) q connection-close?)) c (? . 4)) ((c def c (c (? . 34) q extract-param)) q (24352 . 4)) ((c def c (c (? . 18) q struct:exn:dispatcher)) c (? . 22)) ((c def c (c (? . 16) q set-servlet-handler!)) c (? . 17)) ((c def c (c (? . 1) q responders?)) c (? . 2)) ((c def c (c (? . 1) q host-log-format)) c (? . 12)) ((c def c (c (? . 24) q make)) q (2558 . 4)) ((q def ((lib "web-server/dispatchers/limit.rkt") make)) q (6484 . 5)) ((c def c (c (? . 26) q default-module-specs)) q (5297 . 2)) ((c sig-val c (c (? . 0) q dispatch-server-config^ port)) q (447 . 2)) ((c def c (c (? . 24) q apache-default-format)) q (2403 . 2)) ((c def c (c (? . 1) q host-responders)) c (? . 12)) ((c def c (c (? . 1) q timeouts-file-base)) c (? . 23)) ((c def c (c (? . 9) q path-piece?)) q (25539 . 2)) ((c def c (c (? . 13) q timer-manager?)) q (21925 . 3)) ((c def c (c (? . 14) q make-ssl-connect@)) q (11491 . 6)) ((c form c (c (? . 37) q serial-lambda)) q (23488 . 2)) ((c def c (c (? . 1) q host-table-log-format)) c (? . 8)) ((c def c (c (? . 9) q network-error)) q (25232 . 5)) ((c sig-val c (c (? . 11) q web-config^ max-waiting)) q (14538 . 2)) ((c def c (c (? . 19) q denied?/c)) q (2720 . 2)) ((c form c (c (? . 0) q dispatch-server-connect^)) q (235 . 2)) ((c def c (c (? . 13) q timer-evt)) c (? . 21)) ((c def c (c (? . 6) q url->servlet/c)) q (3839 . 2)) ((c def c (c (? . 1) q host-passwords)) c (? . 12)) ((c def c (c (? . 35) q dispatch-server-with-connect@)) q (865 . 6)) ((c def c (c (? . 1) q paths-passwords)) c (? . 5)) ((c def c (c (? . 16) q servlet-manager)) c (? . 17)) ((c def c (c (? . 39) q gzip/bytes)) q (24447 . 3)) ((c def c (c (? . 9) q read/bytes)) q (25412 . 3)) ((c def c (c (? . 26) q make-stateless.servlet)) q (4945 . 9)) ((c def c (c (? . 1) q messages-servlets-refreshed)) c (? . 15)) ((c def c (c (? . 13) q cancel-timer!)) q (22514 . 3)) ((c def c (c (? . 9) q url-replace-path)) q (24644 . 4)) ((c def c (c (? . 13) q timer-action)) c (? . 21)) ((c def c (c (? . 16) q set-servlet-manager!)) c (? . 17)) ((c def c (c (? . 1) q paths)) c (? . 5)) ((q form ((lib "web-server/private/define-closure.rkt") define-closure)) q (23586 . 2)) ((c def c (c (? . 13) q increment-timer!)) q (22434 . 4)) ((c def c (c (? . 1) q messages?)) c (? . 15)) ((c def c (c (? . 16) q servlet-directory)) c (? . 17)) ((c def c (c (? . 19) q make)) q (2750 . 7)) ((c def c (c (? . 1) q make-configuration-table)) c (? . 7)) ((c def c (c (? . 36) q configuration-table-sexpr->web-config@)) q (15560 . 15)) ((c def c (c (? . 1) q paths-log)) c (? . 5)) ((c form c (c (? . 37) q serial-case-lambda)) q (23531 . 2)) ((c def c (c (? . 31) q make-url->valid-path)) q (1634 . 3)) ((c def c (c (? . 3) q connection-manager?)) q (22817 . 3)) ((c def c (c (? . 38) q dispatch/servlet)) q (11752 . 22)) ((c def c (c (? . 27) q sexpr->configuration-table)) q (16649 . 3)) ((c def c (c (? . 39) q gunzip/bytes)) q (24504 . 3)) ((c def c (c (? . 31) q filter-url->path)) q (1725 . 4)) ((c def c (c (? . 1) q responders-passwords-refreshed)) c (? . 2)) ((c def c (c (? . 18) q next-dispatcher)) q (1488 . 2)) ((q def ((lib "web-server/dispatchers/dispatch-timeout.rkt") make)) q (1925 . 3)) ((c def c (c (? . 16) q set-servlet-namespace!)) c (? . 17)) ((c def c (c (? . 1) q responders-servlets-refreshed)) c (? . 2)) ((c def c (c (? . 14) q serve)) q (6680 . 23)) ((c def c (c (? . 3) q adjust-connection-timeout!)) q (23393 . 4)) ((c def c (c (? . 9) q explode-path*)) q (24866 . 3)) ((c def c (c (? . 13) q struct:timer)) c (? . 21)) ((c def c (c (? . 3) q connection-i-port)) c (? . 4)) ((c def c (c (? . 24) q log-format/c)) q (2448 . 2))))
signature
dispatch-server^ : signature
procedure
(serve)
 -> (->* () (#:confirmation-channel (or/c false/c async-channel?)) (-> void))
procedure
(serve-ports ip op) -> void
  ip : input-port?
  op : output-port?
signature
dispatch-server-connect^ : signature
procedure
(port->real-ports ip op) -> input-port? output-port?
  ip : input-port?
  op : output-port?
signature
dispatch-server-config^ : signature
value
port : listen-port-number?
value
listen-ip : (or/c string? false/c)
value
max-waiting : exact-nonnegative-integer?
value
initial-connection-timeout : integer?
procedure
(read-request c p port-addresses) -> any/c boolean?
  c : connection?
  p : listen-port-number?
  port-addresses : (input-port? . -> . (values string? string?))
value
dispatch : (-> connection? any/c void)
value
dispatch-server-with-connect@
 : (unit/c (import tcp^
                   dispatch-server-connect^
                   dispatch-server-config^)
           (export dispatch-server^))
value
dispatch-server@ : (unit/c (import tcp^
                                   dispatch-server-config^)
                           (export dispatch-server^))
value
dispatcher/c : contract?
procedure
(dispatcher-interface-version/c any) -> boolean?
  any : any/c
struct
(struct exn:dispatcher ()
    #:extra-constructor-name make-exn:dispatcher)
procedure
(next-dispatcher) -> void
value
url->path/c : contract?
procedure
(make-url->path base) -> url->path/c
  base : path-string?
procedure
(make-url->valid-path url->path) -> url->path/c
  url->path : url->path/c
procedure
(filter-url->path regex url->path) -> url->path/c
  regex : regexp?
  url->path : url->path/c
procedure
(make dispatcher ...) -> dispatcher/c
  dispatcher : dispatcher/c
procedure
(make new-timeout) -> dispatcher/c
  new-timeout : integer?
procedure
(make proc) -> dispatcher/c
  proc : (request? . -> . response?)
procedure
(make regex inner) -> dispatcher/c
  regex : regexp?
  inner : dispatcher/c
procedure
(make path proc) -> dispatcher/c
  path : string?
  proc : (request? . -> . response?)
value
format-req/c : contract?
value
paren-format : format-req/c
value
extended-format : format-req/c
value
apache-default-format : format-req/c
value
log-format/c : contract?
procedure
(log-format->format id) -> format-req/c
  id : log-format/c
procedure
(make [#:format format #:log-path log-path]) -> dispatcher/c
  format : format-req/c = paren-format
  log-path : path-string? = "log"
value
denied?/c : contract?
procedure
(make  denied?                                               
      [#:authentication-responder authentication-responder]) 
 -> dispatcher/c
  denied? : denied?/c
  authentication-responder : (url? header? . -> . response?)
                           = (gen-authentication-responder "forbidden.html")
value
authorized?/c : contract?
procedure
(make-basic-denied?/path authorized?) -> denied?/c
  authorized? : authorized?/c
procedure
(password-file->authorized? password-file)
 -> (-> void) authorized?/c
  password-file : path-string?
procedure
(make lookup-dispatcher) -> dispatcher/c
  lookup-dispatcher : (symbol? . -> . dispatcher/c)
procedure
(make  #:url->path url->path                 
      [#:path->mime-type path->mime-type     
       #:indices indices])               -> dispatcher/c
  url->path : url->path/c
  path->mime-type : (path? . -> . (or/c false/c bytes)?)
                  = (lambda (path) #f)
  indices : (listof string?) = (list "index.html" "index.htm")
value
url->servlet/c : contract?
procedure
(make-cached-url->servlet url->path      
                          path->serlvet) 
 -> (-> void) url->servlet/c
  url->path : url->path/c
  path->serlvet : path->servlet/c
procedure
(make  url->servlet                                            
      [#:responders-servlet-loading responders-servlet-loading 
       #:responders-servlet responders-servlet])               
 -> dispatcher/c
  url->servlet : url->servlet/c
  responders-servlet-loading : (url? exn? . -> . can-be-response?)
                             = servlet-loading-responder
  responders-servlet : (url? exn? . -> . can-be-response?)
                     = servlet-error-responder
procedure
(make-v1.servlet directory timeout start) -> servlet?
  directory : path-string?
  timeout : integer?
  start : (request? . -> . can-be-response?)
procedure
(make-v2.servlet directory manager start) -> servlet?
  directory : path-string?
  manager : manager?
  start : (request? . -> . can-be-response?)
procedure
(make-stateless.servlet directory     
                        stuffer       
                        manager       
                        start)    -> servlet?
  directory : path-string?
  stuffer : (stuffer/c serializable? bytes?)
  manager : manager?
  start : (request? . -> . can-be-response?)
value
default-module-specs : (listof module-path?)
value
path->servlet/c : contract?
procedure
(make-default-path->servlet                                                        
                            [#:make-servlet-namespace make-servlet-namespace       
                             #:timeouts-default-servlet timeouts-default-servlet]) 
 -> path->servlet/c
  make-servlet-namespace : make-servlet-namespace/c
                         = (make-make-servlet-namespace)
  timeouts-default-servlet : integer? = 30
value
make-servlet-namespace/c : contract?
procedure
(make-make-servlet-namespace #:to-be-copied-module-specs to-be-copied-module-specs)
 -> make-servlet-namespace/c
  to-be-copied-module-specs : (listof module-path?)
struct
(struct servlet (custodian namespace manager directory handler)
    #:extra-constructor-name make-servlet
    #:mutable)
  custodian : custodian?
  namespace : namespace?
  manager : manager?
  directory : path-string?
  handler : (request? . -> . can-be-response?)
procedure
(make-gc-thread time) -> thread?
  time : integer?
procedure
(make) -> dispatcher/c
procedure
(make limit inner [#:over-limit over-limit]) -> dispatcher/c
  limit : number?
  inner : dispatcher/c
  over-limit : (symbols 'block 'kill-new 'kill-old) = 'block
procedure
(serve                                                            
        #:dispatch dispatch                                       
       [#:confirmation-channel confirmation-channel               
        #:connection-close? connection-close?                     
        #:dispatch-server-connect@ dispatch-server-connect@       
        #:tcp@ tcp@                                               
        #:port port                                               
        #:listen-ip listen-ip                                     
        #:max-waiting max-waiting                                 
        #:initial-connection-timeout initial-connection-timeout]) 
 -> (-> void)
  dispatch : dispatcher/c
  confirmation-channel : (or/c false/c async-channel?) = #f
  connection-close? : boolean? = #f
  dispatch-server-connect@ : (unit/c (import)
                                     (export dispatch-server-connect^))
                           = raw:dispatch-server-connect@
  tcp@ : (unit/c (import) (export tcp^)) = raw:tcp@
  port : listen-port-number? = 80
  listen-ip : (or/c string? false/c) = #f
  max-waiting : integer? = 511
  initial-connection-timeout : integer? = 60
procedure
(serve/ports                                                            
              #:dispatch dispatch                                       
             [#:confirmation-channel confirmation-channel               
              #:connection-close? connection-close?                     
              #:dispatch-server-connect@ dispatch-server-connect@       
              #:tcp@ tcp@                                               
              #:ports ports                                             
              #:listen-ip listen-ip                                     
              #:max-waiting max-waiting                                 
              #:initial-connection-timeout initial-connection-timeout]) 
 -> (-> void)
  dispatch : dispatcher/c
  confirmation-channel : (or/c false/c async-channel?) = #f
  connection-close? : boolean? = #f
  dispatch-server-connect@ : (unit/c (import)
                                     (export dispatch-server-connect^))
                           = raw:dispatch-server-connect@
  tcp@ : (unit/c (import) (export tcp^)) = raw:tcp@
  ports : (listof listen-port-number?) = (list 80)
  listen-ip : (or/c string? false/c) = #f
  max-waiting : integer? = 511
  initial-connection-timeout : integer? = 60
procedure
(serve/ips+ports                                                            
                  #:dispatch dispatch                                       
                 [#:confirmation-channel confirmation-channel               
                  #:connection-close? connection-close?                     
                  #:dispatch-server-connect@ dispatch-server-connect@       
                  #:tcp@ tcp@                                               
                  #:ips+ports ips+ports                                     
                  #:max-waiting max-waiting                                 
                  #:initial-connection-timeout initial-connection-timeout]) 
 -> (-> void)
  dispatch : dispatcher/c
  confirmation-channel : (or/c false/c async-channel?) = #f
  connection-close? : boolean? = #f
  dispatch-server-connect@ : (unit/c (import)
                                     (export dispatch-server-connect^))
                           = raw:dispatch-server-connect@
  tcp@ : (unit/c (import) (export tcp^)) = raw:tcp@
  ips+ports : (listof (cons/c (or/c string? false/c) (listof listen-port-number?)))
            = (list (cons #f (list 80)))
  max-waiting : integer? = 511
  initial-connection-timeout : integer? = 60
procedure
(serve/web-config@                                                      
                    config@                                             
                   [#:dispatch-server-connect@ dispatch-server-connect@ 
                    #:tcp@ tcp@])                                       
 -> (-> void)
  config@ : (unit/c (import) (export web-config^))
  dispatch-server-connect@ : (unit/c (import)
                                     (export dispatch-server-connect^))
                           = raw:dispatch-server-connect@
  tcp@ : (unit/c (import) (export tcp^)) = raw:tcp@
value
raw:dispatch-server-connect@
 : (unit/c (import) (export dispatch-server-connect^))
procedure
(make-ssl-connect@ server-cert-file 
                   server-key-file) 
 -> (unit/c (import) (export dispatch-server-connect^))
  server-cert-file : path-string?
  server-key-file : path-string?
procedure
(do-not-return) -> void
procedure
(dispatch/servlet                                                          
                   start                                                   
                  [#:regexp regexp                                         
                   #:stateless? stateless?                                 
                   #:stuffer stuffer                                       
                   #:manager manager                                       
                   #:current-directory servlet-current-directory           
                   #:responders-servlet-loading responders-servlet-loading 
                   #:responders-servlet responders-servlet])               
 -> dispatcher/c
  start : (request? . -> . response?)
  regexp : regexp? = #rx""
  stateless? : boolean? = #f
  stuffer : (stuffer/c serializable? bytes?) = default-stuffer
  manager : manager?
          = (make-threshold-LRU-manager #f (* 1024 1024 64))
  servlet-current-directory : path-string? = (current-directory)
  responders-servlet-loading : (url? any/c . -> . can-be-response?)
                             = servlet-loading-responder
  responders-servlet : (url? any/c . -> . can-be-response?)
                     = servlet-error-responder
procedure
(serve/launch/wait  make-dispatcher                       
                   [#:connection-close? connection-close? 
                    #:launch-path launch-path             
                    #:banner? banner?                     
                    #:listen-ip listen-ip                 
                    #:port port                           
                    #:max-waiting max-waiting             
                    #:ssl-cert ssl-cert                   
                    #:ssl-key ssl-key])                   
 -> void
  make-dispatcher : (semaphore? . -> . dispatcher/c)
  connection-close? : boolean? = #f
  launch-path : (or/c false/c string?) = #f
  banner? : boolean? = #f
  listen-ip : (or/c false/c string?) = "127.0.0.1"
  port : number? = 8000
  max-waiting : exact-nonnegative-integer? = 511
  ssl-cert : (or/c false/c path-string?) = #f
  ssl-key : (or/c false/c path-string?) = #f
signature
web-server^ : signature
procedure
(serve) -> (-> void)
procedure
(serve-ports ip op) -> void
  ip : input-port?
  op : output-port?
value
web-server-with-connect@
 : (unit/c (web-config^ tcp^ dispatch-server-connect^)
           (web-server^))
value
web-server@ : (unit/c (web-config^ tcp^)
                      (web-server^))
signature
web-config^ : signature
value
max-waiting : exact-nonnegative-integer?
value
virtual-hosts : (string? . -> . host?)
value
initial-connection-timeout : integer?
value
port : port-number?
value
listen-ip : (or/c false/c string?)
value
make-servlet-namespace : make-servlet-namespace/c
procedure
(configuration-table->web-config@                                                    
                                   path                                              
                                  [#:port port                                       
                                   #:listen-ip listen-ip                             
                                   #:make-servlet-namespace make-servlet-namespace]) 
 -> (unit/c (import) (export web-config^))
  path : path-string?
  port : (or/c false/c port-number?) = #f
  listen-ip : (or/c false/c string?) = #f
  make-servlet-namespace : make-servlet-namespace/c
                         = (make-make-servlet-namespace)
procedure
(configuration-table-sexpr->web-config@                                                    
                                         sexpr                                             
                                        [#:web-server-root web-server-root                 
                                         #:port port                                       
                                         #:listen-ip listen-ip                             
                                         #:make-servlet-namespace make-servlet-namespace]) 
 -> (unit/c (import) (export web-config^))
  sexpr : list?
  web-server-root : path-string?
                  = (directory-part default-configuration-table-path)
  port : (or/c false/c port-number?) = #f
  listen-ip : (or/c false/c string?) = #f
  make-servlet-namespace : make-servlet-namespace/c
                         = (make-make-servlet-namespace)
value
default-configuration-table-path : path?
value
configuration-table-sexpr? : (any . -> . boolean?)
procedure
(sexpr->configuration-table sexpr) -> configuration-table?
  sexpr : configuration-table-sexpr?
procedure
(configuration-table->sexpr ctable)
 -> configuration-table-sexpr?
  ctable : configuration-table?
procedure
(read-configuration-table path) -> configuration-table?
  path : path-string?
procedure
(write-configuration-table ctable path) -> void
  ctable : configuration-table?
  path : path-string?
struct
(struct configuration-table (port
                             max-waiting
                             initial-connection-timeout
                             default-host
                             virtual-hosts)
    #:extra-constructor-name make-configuration-table)
  port : port-number?
  max-waiting : exact-nonnegative-integer?
  initial-connection-timeout : natural-number/c
  default-host : host-table?
  virtual-hosts : (listof (cons/c string? host-table?))
struct
(struct host-table (indices log-format messages timeouts paths)
    #:extra-constructor-name make-host-table)
  indices : (listof string?)
  log-format : symbol?
  messages : messages?
  timeouts : timeouts?
  paths : paths?
struct
(struct host (indices
              log-format
              log-path
              passwords
              responders
              timeouts
              paths)
    #:extra-constructor-name make-host)
  indices : (listof string?)
  log-format : symbol?
  log-path : (or/c false/c path-string?)
  passwords : (or/c false/c path-string?)
  responders : responders?
  timeouts : timeouts?
  paths : paths?
struct
(struct responders (servlet
                    servlet-loading
                    authentication
                    servlets-refreshed
                    passwords-refreshed
                    file-not-found
                    protocol
                    collect-garbage)
    #:extra-constructor-name make-responders)
  servlet : (url? any/c . -> . response?)
  servlet-loading : (url? any/c . -> . response?)
  authentication : (url? (cons/c symbol? string?) . -> . response?)
  servlets-refreshed : (-> response?)
  passwords-refreshed : (-> response?)
  file-not-found : (request? . -> . response?)
  protocol : (url? . -> . response?)
  collect-garbage : (-> response?)
struct
(struct messages (servlet
                  authentication
                  servlets-refreshed
                  passwords-refreshed
                  file-not-found
                  protocol
                  collect-garbage)
    #:extra-constructor-name make-messages)
  servlet : string?
  authentication : string?
  servlets-refreshed : string?
  passwords-refreshed : string?
  file-not-found : string?
  protocol : string?
  collect-garbage : string?
struct
(struct timeouts (default-servlet
                  password
                  servlet-connection
                  file-per-byte
                  file-base)
    #:extra-constructor-name make-timeouts)
  default-servlet : number?
  password : number?
  servlet-connection : number?
  file-per-byte : number?
  file-base : number?
struct
(struct paths (conf
               host-base
               log
               htdocs
               servlet
               mime-types
               passwords)
    #:extra-constructor-name make-paths)
  conf : (or/c false/c path-string?)
  host-base : (or/c false/c path-string?)
  log : (or/c false/c path-string?)
  htdocs : (or/c false/c path-string?)
  servlet : (or/c false/c path-string?)
  mime-types : (or/c false/c path-string?)
  passwords : (or/c false/c path-string?)
procedure
(file-response http-code         
               short-version     
               text-file         
               header ...)   -> response?
  http-code : natural-number/c
  short-version : string?
  text-file : string?
  header : header?
procedure
(servlet-loading-responder url exn) -> response?
  url : url?
  exn : exn?
procedure
(gen-servlet-not-found file) -> ((url url?) . -> . response?)
  file : path-string?
procedure
(servlet-error-responder url exn) -> response?
  url : url?
  exn : exn?
procedure
(gen-servlet-responder file)
 -> ((url url?) (exn any/c) . -> . response?)
  file : path-string?
procedure
(gen-servlets-refreshed file) -> (-> response?)
  file : path-string?
procedure
(gen-passwords-refreshed file) -> (-> response?)
  file : path-string?
procedure
(gen-authentication-responder file)
 -> ((url url?) (header header?) . -> . response?)
  file : path-string?
procedure
(gen-protocol-responder file) -> ((url url?) . -> . response?)
  file : path-string?
procedure
(gen-file-not-found-responder file)
 -> ((req request?) . -> . response?)
  file : path-string?
procedure
(gen-collect-garbage-responder file) -> (-> response?)
  file : path-string?
procedure
(timer-manager? x) -> boolean?
  x : any/c
struct
(struct timer (tm evt expire-seconds action)
    #:extra-constructor-name make-timer)
  tm : timer-manager?
  evt : evt?
  expire-seconds : number?
  action : (-> void)
procedure
(start-timer-manager) -> timer-manager?
procedure
(start-timer tm s action) -> timer?
  tm : timer-manager?
  s : number?
  action : (-> void)
procedure
(reset-timer! t s) -> void
  t : timer?
  s : number?
procedure
(increment-timer! t s) -> void
  t : timer?
  s : number?
procedure
(cancel-timer! t) -> void
  t : timer?
struct
(struct connection (timer i-port o-port custodian close?)
    #:extra-constructor-name make-connection)
  timer : timer?
  i-port : input-port?
  o-port : output-port?
  custodian : custodian?
  close? : boolean?
procedure
(connection-manager? x) -> boolean?
  x : any/c
procedure
(start-connection-manager) -> connection-manager?
procedure
(new-connection cm          
                timeout     
                i-port      
                o-port      
                cust        
                close?) -> connection?
  cm : connection-manager?
  timeout : number?
  i-port : input-port?
  o-port : output-port?
  cust : custodian?
  close? : boolean?
procedure
(kill-connection! c) -> void
  c : connection?
procedure
(adjust-connection-timeout! c t) -> void
  c : connection?
  t : number?
syntax
(serial-lambda formals body ...)
syntax
(serial-case-lambda [formals body ...] ...)
syntax
(define-closure tag formals (free-var ...) body)
procedure
(make-cache-table) -> cache-table?
procedure
(cache-table-lookup! ct id mk) -> any/c
  ct : cache-table?
  id : symbol?
  mk : (-> any/c)
procedure
(cache-table-clear! ct) -> void?
  ct : cache-table?
procedure
(cache-table? v) -> boolean?
  v : any/c
procedure
(read-mime-types p) -> (hash/c symbol? bytes?)
  p : path-string?
procedure
(make-path->mime-type p) -> (path? . -> . (or/c false/c bytes?))
  p : path-string?
procedure
(compress-serial sv) -> list?
  sv : list?
procedure
(decompress-serial csv) -> list?
  csv : list?
procedure
(insert-param u k v) -> url?
  u : url?
  k : string?
  v : string?
procedure
(extract-param u k) -> (or/c string? false/c)
  u : url?
  k : string?
procedure
(gzip/bytes ib) -> bytes?
  ib : bytes?
procedure
(gunzip/bytes ib) -> bytes?
  ib : bytes?
procedure
(bytes-ci=? b1 b2) -> boolean?
  b1 : bytes?
  b2 : bytes?
procedure
(url-replace-path proc u) -> url?
  proc : ((listof path/param?) . -> . (listof path/param?))
  u : url?
procedure
(url-path->string url-path) -> string?
  url-path : (listof path/param?)
procedure
(explode-path* p) -> (listof path-piece?)
  p : path-string?
procedure
(path-without-base base p) -> (listof path-piece?)
  base : path-string?
  p : path-string?
procedure
(directory-part p) -> path?
  p : path-string?
procedure
(build-path-unless-absolute base p) -> path?
  base : path-string?
  p : path-string?
procedure
(network-error s fmt v ...) -> void
  s : symbol?
  fmt : string?
  v : any/c
procedure
(exn->string exn) -> string?
  exn : (or/c exn? any/c)
procedure
(read/bytes bstr) -> printable/c
  bstr : bytes?
procedure
(write/bytes v) -> bytes?
  v : printable/c
value
path-piece? : contract?
