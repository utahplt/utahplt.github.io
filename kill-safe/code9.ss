(module code9 mzscheme
  (require (lib "cml.ss")
           (lib "list.ss")
           (lib "contract.ss"))
  
  (define-struct q (in-ch req-ch mgr-t))
  ;; make-q : alpha-channel alpha-req-channel thread -> alpha-queue
  ;; q-in-ch : alpha-queue -> alpha-channel
  ;; q-req-ch : alpha-queue -> alpha-req-channel
  ;; q-mgr-t : alpha-queue -> thread
  
  (define-struct req (pred out-ch gave-up-evt))
  ;; make-req : (alpha -> bool) alpha-channel void-event -> alpha-req
  ;; req-pred : alpha-req -> alpha -> bool
  ;; req-out-ch : alpha-req -> alpha-channel
  ;; req-gave-up-evt : alpha-req -> void-event
  
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
        ;; Search queue items using pred
        (find-first-item 
         (req-pred req)
         items
         (lambda (item)
           ;; Found an item; try to service req
           (wrap-evt 
            (channel-send-evt (req-out-ch req) item)
            (lambda (void)
              ;; Serviced, so remove item and request
              (serve (remove item items)
                     (remove req reqs)))))
         (lambda ()
           ;; No matching item to service req
           never-evt))))
    
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
       ;; and send the server gave-up-evt
       (sync (channel-send-evt 
              (q-req-ch q) 
              (make-req pred out-ch gave-up-evt)))
       ;; Result arrives on out-ch
       (channel-recv-evt out-ch))))
  
  ;; This function doesn't appear in the paper
  (define (find-first-item pred items found-k none-k)
    (cond
      [(null? items) (none-k)]
      [(pred (car items)) (found-k (car items))]
      [else (find-first-item pred (cdr items) found-k none-k)])))