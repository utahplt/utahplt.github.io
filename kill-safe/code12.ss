(module code12 mzscheme
  (require (lib "cml.ss")
           (lib "contract.ss"))
  
  (define-struct sc (ch mgr-t))
  ;; make-sc : alpha-req-channel thread -> alpha-swap-channel
  
  (define-struct req (v ch gave-up))
  ;; make-req : alpha alpha-channel void-event -> alpha-req
  
  (provide/contract
   [sc? (-> any? boolean?)]
   [swap-channel (-> sc?)]
   [swap-evt (sc? any? . -> . object-waitable?)])
  
  (define (swap-channel)
    (define ch (channel))
    (define (serve-first)
      ;; Get first thread for swap
      (sync (wrap-evt
             (channel-recv-evt ch)
             serve-second)))
    (define (serve-second a)
      ;; Try to get second thread for swap
      (sync
       (choice-evt
        ;; Possibility 1 --- got second thread, so swap
        (wrap-evt
         (channel-recv-evt ch)
         (lambda (b)
           ;; Send each thread the other's value
           (send-eventually (req-ch a) (req-v b))
           (send-eventually (req-ch b) (req-v a))
           (serve-first)))
        ;; Possibility 2 --- first gave up, so start over
        (wrap-evt
         (req-gave-up a)
         (lambda (void) (serve-first))))))
    (define (send-eventually ch v)
      ;; Spawn a thread, in case ch's thread isn't ready
      (spawn (lambda () 
               (sync (channel-send-evt ch v)))))
    (make-sc ch (spawn serve-first)))
  
  (define (swap-evt sc v)
    (nack-guard-evt
     (lambda (gave-up)
       (define in-ch (channel)) 
       (thread-resume (sc-mgr-t sc) (current-thread))
       (sync 
        (wrap-evt
         (channel-send-evt (sc-ch sc) 
                           (make-req v in-ch gave-up))
         (lambda (void) in-ch)))))))