(module code7 mzscheme
  (require (lib "cml.ss")
           (lib "contract.ss"))
  
  (define-struct q (in-ch out-ch mgr-t))
  ;; make-q : alpha-channel alpha-channel thread -> alpha-queue
  ;; q-in-ch : alpha-queue -> alpha-channel
  ;; q-out-ch : alpha-queue -> alpha-channel
  ;; q-mgr-t : alpha-queue -> thread
  
  (provide/contract
   [q? (any? . -> . boolean?)]
   [queue (-> q?)]
   [queue-send-evt (q? any? . -> . object-waitable?)]
   [queue-recv-evt (q? . -> . object-waitable?)])
  
  (define (queue)
    (define in-ch (channel)) ; to accept sends into queue
    (define out-ch (channel)) ; to supply recvs from queue
    ;; A manager thread loops with serve
    (define (serve items)
      (if (null? items)
          ;; Nothing to supply a recv until we accept a send
          (serve (list (sync (channel-recv-evt in-ch))))
          ;; Accept a send or supply a recv, whichever is ready
          (sync (choice-evt
                 (wrap-evt
                  (channel-recv-evt in-ch)
                  (lambda (v)
                    ;; Accepted a send; enqueue it
                    (serve (append items (list v)))))
                 (wrap-evt
                  (channel-send-evt out-ch (car items))
                  (lambda (void)
                    ;; Supplied a recv; dequeue it
                    (serve (cdr items))))))))
    ;; Create the manager thread
    (define mgr-t (spawn (lambda () (serve (list)))))
    ;; Return a queue as an opaque q record
    (make-q in-ch out-ch mgr-t))
  
  (define (queue-send-evt q v)
    (guard-evt 
     (lambda ()
       ;; Make sure the manager thread runs
       (thread-resume (q-mgr-t q) (current-thread))
       ;; Channel send
       (channel-send-evt (q-in-ch q) v))))
  
  (define (queue-recv-evt q)
    (guard-evt 
     (lambda ()
       ;; Make sure the manager thread runs
       (thread-resume (q-mgr-t q) (current-thread))
       ;; Channel receive
       (channel-recv-evt (q-out-ch q))))))
