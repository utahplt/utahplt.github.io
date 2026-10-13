(module test mzscheme
  (require (lib "cml.ss"))
  (provide test show-results reset-counters)
  
  (define total-tests 0)
  (define failed-tests 0)
  
  (define (show-results)
    (printf "~a total tests, ~a failed\n" total-tests failed-tests))
  
  (define (reset-counters)
    (set! total-tests 0)
    (set! failed-tests 0))
  
  (define (inc-total-tests) (set! total-tests (+ total-tests 1)))
  (define (inc-failed-tests) (set! failed-tests (+ failed-tests 1)))
  
  ;; (test actual expected) = (run-test (lambda () actual) expected <line> <col>)
  (define-syntax (test stx)
    (syntax-case stx ()
      [(_ actual expected)
       (with-syntax ([line (syntax-line stx)]
                     [col (syntax-column stx)])
         (syntax
          (run-test (lambda () actual) expected line col)))]))
  
  ;; Calls actual in a new thread with a new custodian
  ;; with a 3 second timeout. If the timeout doesn't occur
  ;; and the test and acutal are the same, the test passes.
  ;; otherwise it does not. In either case, shuts the custodian down.
  (define (run-test actual-thunk expected line col)
    (let ([c-ans (make-channel)]
          [c-timeout (make-channel)]
          [cust (make-custodian)])
      (inc-total-tests)
      (parameterize ([current-custodian cust])
        (spawn (lambda () (channel-put c-ans (actual-thunk)))))
      (spawn (lambda () (sleep 3) (channel-put c-timeout (void))))
      (let/ec k
        (let ([actual
               (sync (choice-evt
                      (channel-recv-evt c-ans)
                      (wrap-evt
                       c-timeout
                       (lambda (v)
                         (failed line col "timeout expired")
                         (k (void))))))])
          (custodian-shutdown-all cust)
          (unless (equal? expected actual)
            (failed line col
                    "expected ~s got ~s\n" 
                    expected 
                    actual))))))
  
  (define (failed line col fmt . args)
    (inc-failed-tests)
    (display (string-append (format "failed test ~a:~a " line col)
                            (apply format fmt args)))
    (newline)))