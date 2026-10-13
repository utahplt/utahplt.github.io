;; SRFI 9

(require-library "synrule.ss")

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor more ...) ...)

     (begin
       (define-record-procs type (field-tag ...)
	 ((accessor more ...) ...)
	 (type
	  real-constructor
	  predicate))
       (define (constructor constructor-tag ...)
	 (record-constructor-call real-constructor
				  (constructor-tag ...)
				  (field-tag ...)))))))

(define-macro record-constructor-call
  (lambda (real-constructor constructor-tags field-tags)
    (let loop ((constructor-tags constructor-tags)
	       (field-tags field-tags)
	       (reverse-result '()))
      (cond
       ((and (null? constructor-tags)
	     (null? field-tags))
	`(, real-constructor . ,(reverse reverse-result)))
       ((null? field-tags)
	(raise-syntax-error 'define-record-type
			    "invalid constructor syntax"))
       ((and (not (null? constructor-tags))
	     (eq? (car constructor-tags)
		  (car field-tags)))
	(loop (cdr constructor-tags)
	      (cdr field-tags)
	      (cons (car constructor-tags) reverse-result)))
       (else
	(loop constructor-tags
	      (cdr field-tags)
	      (cons '(%void) reverse-result)))))))

;; (define-syntax record-constructor-call
;;   (syntax-rules ()
;;     ((record-constructor-call real-constructor
;; 			      ()
;; 			      ()
;; 			      result)
;;      (real-constructor . result))
;;     ((record-constructor-call real-constructor
;; 			      (tag constructor-tags ...)
;; 			      (tag field-tags ...)
;; 			      (result ...))
;;      (record-constructor-call real-constructor
;; 			      (constructor-tags ...)
;; 			      (field-tags ...)
;; 			      (result ... tag)))
;;     ;; Unfortunately, R5RS doesn't really verify that TAG and TAG in
;;     ;; the above macro are equal.  Hence, we have to do without and
;;     ;; can only support a subset of the SRFI
;; 
;; ;;    ((record-constructor-call real-constructor
;; ;;			      (constructor-tags ...)
;; ;;			      (field-tag field-tags ...)
;; ;;			      (result ...))
;; ;;     (record-constructor-call real-constructor
;; ;;			      (constructor-tags ...)
;; ;;			      (field-tags ...)
;; ;;			      (result ... #f)))
;; 
;;     ))

(define-syntax define-record-procs
  (syntax-rules ()
    ((define-record-procs type field-tags () result)
     (define-values result (struct type field-tags)))
    ((define-record-procs type field-tags
       ((accessor) rest ...) (stuff ...))
     (define-record-procs type field-tags (rest ...)
       (stuff ... accessor no-modifier)))
    ((define-record-procs type field-tags
       ((accessor modifier) rest ...) (stuff ...))
     (define-record-procs type field-tags (rest ...)
       (stuff ... accessor modifier)))))
