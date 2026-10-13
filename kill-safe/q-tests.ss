(module q-tests mzscheme
  (require (lib "cml.ss") 
           "test.ss"
           "code7.ss")
  
  (reset-counters)
  
  ;; Test basic queue properties
  (test (let ([q (queue)])
          (sync (queue-send-evt q 1))
          (sync (queue-recv-evt q)))
        1)
  
  (test (let ([q (queue)])
          (sync (queue-send-evt q 1))
          (sync (queue-send-evt q 2))
          (list (sync (queue-recv-evt q))
                (sync (queue-recv-evt q))))
        (list 1 2))
  
  ;; Test queue choice
  (test (let ([q (queue)])
          (sync (queue-send-evt q 5))
          (sync 
           (choice-evt
            (queue-recv-evt q)
            (queue-recv-evt q))))
        5)
  
  (test (let ([q (queue)])
          (sync (queue-send-evt q 5))
          (sync (queue-send-evt q 5))
          (list (sync 
                 (choice-evt
                  (queue-recv-evt q)
                  (queue-recv-evt q)))
                (sync (queue-recv-evt q))))
        (list 5 5))
  
  ;; Test kill while waiting
  (test (let ([q (queue)])
          (kill-thread 
           (spawn
            (lambda ()
              (sync (queue-recv-evt q)))))
          (sync (queue-send-evt q 1))
          (sync (queue-recv-evt q)))
        1)
  
  (show-results))