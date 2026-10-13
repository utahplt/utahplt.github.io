(module code11 mzscheme
  (require (lib "cml.ss")
           (lib "contract.ss"))
  
  (define-struct sc (ch))
  ;; make-sc : alpha-req-channel -> alpha-swap-channel
  ;; sc-ch : alpha-swap-channel -> alpha-req-channel
  
  (define-struct req (v ch))
  ;; make-req : alpha alpha-channel -> alpha-req
  ;; req-v : alpha-req -> alpha
  ;; req-ch : alpha-req -> alpha-channel
  
  (provide/contract
   [sc? (-> any? boolean?)]
   [swap-channel (-> sc?)]
   [swap-evt (sc? any? . -> . object-waitable?)])
  
  (define (swap-channel) (make-sc (channel)))

  (define (swap-evt sc v)
    (guard-evt
     (lambda ()
       (define in-ch (channel))
       (choice-evt
        ;; Maybe act as server and receive req
        (wrap-evt
         (channel-recv-evt (sc-ch sc))
         (lambda (req)
           ;; Reply to req
           (sync (channel-send-evt (req-ch req) v))
           (req-v req)))
        ;; Maybe act as client and send req
        (wrap-evt
         (channel-send-evt (sc-ch sc) (make-req v in-ch))
         (lambda (void)
           ;; Receive answer to req
           (sync (channel-recv-evt in-ch)))))))))
