(module code10 mzscheme
  (require (lib "cml.ss")
           (lib "list.ss")
           (lib "contract.ss"))
  
  (define-struct q (in-ch req-ch mgr-t))
  ;; make-q : alpha-channel alpha-req-channel thread -> alpha-queue
  ;; q-in-ch : alpha-queue -> alpha-channel
  ;; q-req-ch : alpha-queue -> alpha-req-channel
  ;; q-mgr-t : alpha-queue -> thread
  
  (define-struct req (pred out-ch gave-up-evt cust ok-items))
  ;; make-req : (alpha -> bool) alpha-channel void-event custodian alpha-list -> alpha-req
  ;; req-pred : alpha-req -> alpha -> bool
  ;; req-out-ch : alpha-req -> alpha-channel
  ;; req-gave-up-evt : alpha-req -> void-event
  ;; req-cust : alpha-req -> custodian 
  ;; req-ok-items : alpha-req -> alpha-list 
  
  (provide/contract [q? (any? . -> . boolean?)]
                    [msg-queue (-> q?)]
                    [msg-queue-send-evt (q? any? . -> . object-waitable?)]
                    [msg-queue-recv-evt (q? (any? . -> . boolean?) . -> . any)])
  
  (define (msg-queue)
    (define in-ch (channel))
    (define req-ch (channel))
    (define never-evt (channel-recv-evt (channel)))
    (define (serve items reqs)
      (sync (apply
             choice-evt
             ;; Maybe accept a send
             (wrap-evt 
              (channel-recv-evt in-ch)
              (lambda (v)
                ;; Accepted a send; enqueue it
                (serve (append items (list v)) reqs)))
             ;; Maybe accept a recv request
             (wrap-evt
              (channel-recv-evt req-ch)
              (lambda (req) 
                ;; Accepted a recv request; add it
                (serve items (cons req reqs))))
             (append
              ;; Maybe service a recv request in reqs
              (map (make-service-evt items reqs) reqs)
              ;; Maybe give up on a request
              (map (make-abandon-evt items reqs) reqs)))))
    
    (define (make-service-evt items reqs)
      (lambda (req)
        (if (null? (req-ok-items req))
            ;; Look for items acceptable to pred
            (wrap-evt
             (ok-items-evt req items)
             (lambda (ok-items)
               ;; Got a list of acceptable items, so update req
               (serve items
                      (cons (new-ok-items req ok-items)
                            (remove req reqs)))))
            ;; Use first acceptable item to service req
            (wrap-evt 
             (channel-send-evt (req-out-ch req)
                               (car (req-ok-items req)))
             (lambda (void)
               ;; Serviced, so remove item and request
               (define item (car (req-ok-items req)))
               (serve (remove item items)
                      (map
                       (remove-ok-item item)
                       (remove req reqs))))))))
    (define (ok-items-evt req items)
      ;; New thread runs pred and delivers a list to items-ch
      (define items-ch (channel))
      (with-handlers ([(lambda (x) #t) 
                       (lambda (exn)
                         ;; this runs when custodian is somehow shutdown
                         (spawn 
                          (lambda () (channel-put items-ch '()))))])
        (parameterize ([current-custodian (req-cust req)])
          (spawn
           (lambda ()
             (define ok-items (filter (req-pred req) items))
             (sync (channel-send-evt items-ch ok-items))))))
      (channel-recv-evt items-ch))
    (define (remove-ok-item item)
      ;; Given a req, remove item from its list of acceptable items
      (lambda (req)
        (new-ok-items req
                      (remove item (req-ok-items req)))))
    (define (new-ok-items req ok-items)
      (make-req (req-pred req) (req-out-ch req)
                (req-gave-up-evt req) (req-cust req)
                ok-items))
    
    
    (define (make-abandon-evt items reqs)
      (lambda (req)
        ;; Event to detect that the receiver gives up
        (wrap-evt (req-gave-up-evt req)
                  (lambda (void)
                    ;; Receiver gave up; remove request
                    (serve items (remove req reqs))))))
    
    (define mgr-t (spawn (lambda () (serve (list) (list)))))
    
    (make-q in-ch req-ch mgr-t))
  
  (define (msg-queue-send-evt q v)
    (guard-evt 
     (lambda ()
       ;; Make sure the manager thread runs
       (thread-resume (q-mgr-t q) (current-thread))
       ;; Channel send
       (channel-send-evt (q-in-ch q) v))))
  
  (define (msg-queue-recv-evt q pred)
    (nack-guard-evt 
     (lambda (gave-up-evt)
       (define out-ch (channel))
       ;; Make sure the manager thread runs
       (thread-resume (q-mgr-t q) (current-thread))
       ;; Request for an item matching pred with reply to out-ch
       ;; and send the server gave-up-evt, plus a custodian and 
       ;; an initially empty list of known acceptable items
       (sync (channel-send-evt 
              (q-req-ch q) 
              (make-req pred out-ch gave-up-evt
                        (current-custodian) (list))))
       ;; Result arrives on out-ch
       (channel-recv-evt out-ch))))
  
  ;; This function doesn't appear in the paper
  (define (find-first-item pred items found-k none-k)
    (cond
      [(null? items) (none-k)]
      [(pred (car items)) (found-k (car items))]
      [else (find-first-item pred (cdr items) found-k none-k)])))