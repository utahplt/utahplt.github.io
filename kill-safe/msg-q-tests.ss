(module msg-q-tests mzscheme
  (require (lib "cml.ss") 
           (lib "list.ss")
           "test.ss"
           (prefix code9: "code9.ss")
           (prefix code10: "code10.ss"))
  
  (define (run-msg-tests msg-queue msg-queue-send-evt msg-queue-recv-evt)
    (reset-counters)
    
    ;; Test basic queue properties
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 1))
            (sync (msg-queue-recv-evt q any?)))
          1)
    
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 1))
            (sync (msg-queue-send-evt q 2))
            (list (sync (msg-queue-recv-evt q any?))
                  (sync (msg-queue-recv-evt q any?))))
          (list 1 2))
    
    ;; Test filter
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 1))
            (sync (msg-queue-send-evt q 2))
            (list (sync (msg-queue-recv-evt q two?))
                  (sync (msg-queue-recv-evt q one?))))
          (list 2 1))
    
    ;; Test queue choice
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 5))
            (sync 
             (choice-evt
              (msg-queue-recv-evt q any?)
              (msg-queue-recv-evt q any?))))
          5)
    
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 5))
            (sync (msg-queue-send-evt q 5))
            (list (sync 
                   (choice-evt
                    (msg-queue-recv-evt q any?)
                    (msg-queue-recv-evt q any?)))
                  (sync (msg-queue-recv-evt q any?))))
          (list 5 5))
    
    ;; Test kill while waiting
    (test (let ([q (msg-queue)])
            (kill-thread 
             (spawn
              (lambda ()
                (sync (msg-queue-recv-evt q any?)))))
            (sync (msg-queue-send-evt q 1))
            (sync (msg-queue-recv-evt q any?)))
          1)
    
    ;; Test blocked filter
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 1))
            (msg-queue-recv-evt 
             q
             (lambda (x)
               (sync (channel-recv-evt (make-channel)))))
            (sleep 1)
            (sync (msg-queue-recv-evt q any?)))
          1)
    
    ;; Test suicidal code
    ;; This test fails for the code from figure 9, but figure 10 handles it properly
    (test (let ([q (msg-queue)])
            (sync (msg-queue-send-evt q 1))
            
            ;; rogue suicidal code -- should only be able to kill itself.
            (parameterize ([current-custodian (make-custodian)])
              (spawn
               (lambda ()
                 (sync 
                  (msg-queue-recv-evt
                   q 
                   (let ([die? #t])
                     (lambda (x) 
                       (when die?
                         (set! die? #f)
                         (custodian-shutdown-all (current-custodian)))
                       #t)))))))
            
            (sync (msg-queue-send-evt q 1))
            (sleep 1)
            (sync (msg-queue-recv-evt q any?)))
          1)
    
    (test (let* ([q (msg-queue)]
                 [get-evt
                  (choice-evt
                   (msg-queue-recv-evt q odd?)
                   (msg-queue-recv-evt q even?))])
            (sync (msg-queue-send-evt q 1))
            (sync (msg-queue-send-evt q 2))
            
            ;; With figure 9's code, the following expression makes the
            ;; syncs in the next exps usually get stuck (but not always)
            (spawn (lambda ()
                     (sync (msg-queue-recv-evt 
                            q 
                            (lambda (x) 
                              (kill-thread (current-thread))
                              #t)))))
            
            (quicksort (list (sync get-evt) (sync get-evt)) <=))
          (list 1 2))
        
    (show-results))
  
  (define (any? x) #t)
  (define (one? x) (= x 1))
  (define (two? x) (= x 2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; run the tests
  ;;

  (run-msg-tests code9:msg-queue
                 code9:msg-queue-send-evt
                 code9:msg-queue-recv-evt)
  
  (run-msg-tests code10:msg-queue
                 code10:msg-queue-send-evt
                 code10:msg-queue-recv-evt))
