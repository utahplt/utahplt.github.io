;; requires list-library, SRFI-1 for append-map, memq, reverse!, etc.
;; see http://srfi.schemers.org/srfi-1
;; 

(require-library "pretty.ss")
(require-library "file.ss")

;; --
;; CHECK-VARIABLES
;; author: Will Fitzgerald. Neodesic Corporation.
;; March, 2000.
;; --
;; Checks expressions, looking for undefined
;; variables (including undefined procedures).
;;
;; To use, first load all definitions,
;; then run CHECK-VARIABLES on each expression of
;; interest.
;;
;; For example:
;; > (define (testing a b) (+ a b c)) ; <- is C global? 
;; > (check-variables '(define (testing a b) (+ a b c)))
;; ;; WARNING: c IS NOT DEFINED IN (define (testing a b) (+ a b c))
;;
;; You can also do (CHECK-VARIABLES-IN-FILE filename)
;; and (CHECK-VARIABLES-IN-FILES list-of-filenames)
;;
;; This is all very MzScheme specific, using my best
;; guesses at what EXPAND-DEFMACRO does, and I'm likely
;; to have missed some cases. If I have, please let me know: 
;; fitzgerald@neodesic.com
;; --
;; Copyright (c) 2000. Neodesic Corporation.
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this file to deal in this file without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the file,
;; and to permit persons to whom the file is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of this file.
;;
;; THE FILE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE FILE OR THE USE OR OTHER DEALINGS IN THE FILE.
;; --

(define (globally-defined? x)
  (with-handlers ((exn:variable? (lambda (exn) #f))) 
		 (begin (global-defined-value x) #t)))

(define (warn-if-not-defined master-expr symbol vars)
  (if (not (or (memq symbol vars)
	       (globally-defined? symbol)))
      (begin
	(display-all ";; WARNING: " symbol " IS NOT DEFINED IN ")
	(pretty-print master-expr))))

(define (collect-%let-vars let-definition)
   (append-map car (cadr let-definition)))

(define (collect-%letrec-vars let-definition)
  (append-map car (cadr let-definition)))

(define (collect-formals lambda-definition)
  (cond
   ((null? (cadr lambda-definition)) '())
   ((symbol? (cadr lambda-definition))
    (list (cadr lambda-definition)))
   (ELSE
    (reverse! ; not really needed, but suits my sense of order
     (let loop ((formal-def (cadr lambda-definition))
		(formals '()))
       (cond
	((and (not (pair? (cdr formal-def)))
	      (symbol? (cdr formal-def)))
	 (cons (cdr formal-def) (cons (car formal-def) formals)))
	((null? (cdr formal-def))
	 (cons (car formal-def) formals))
	(else
	 (loop (cdr formal-def) (cons (car formal-def) formals)))))))))
   
;; assume macro-expanded.
(define (check-variables* master-expr expr vars)
  (cond
   ((symbol? expr) 
    (warn-if-not-defined master-expr expr vars))
   ((atom? expr) (values))
   ((pair? (car expr))
    (check-variables* master-expr (car expr) vars)
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      (cdr vars)))
   ((eq? (car expr) '#%QUOTE) (values))
   ((eq? (car expr) '#%BEGIN)
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      (cdr expr)))
   ((eq? (car expr) '#%SET!)
    (warn-if-not-defined master-expr (cadr expr) vars)
    (check-variables* master-expr (caddr expr) vars))
   ((eq? (car expr) '#%IF)
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      (cdr expr)))
   ((eq? (car expr) '#%LAMBDA)
    (let ((new-vars (append (collect-formals expr) vars)))
      (for-each (lambda (subexpr)
		  (check-variables* master-expr subexpr new-vars))
		(cddr expr))))
   ((eq? (car expr) '#%LETREC-VALUES)
    (let ((new-vars (append (collect-%letrec-vars expr) vars)))
      (for-each (lambda (subexpr)
		  (check-variables* master-expr subexpr new-vars))
		(cddr expr))))
   ((eq? (car expr) '#%LET-VALUES)
    (let ((new-vars (append (collect-%let-vars expr) vars)))
      (for-each (lambda (subexpr)
		  (check-variables* master-expr subexpr new-vars))
		(cddr expr))))
   ((eq? (car expr) '#%DEFINE)
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      (cddr expr)))
   ((eq? (car expr) '#%DEFINE-MACRO)
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      (cddr expr)))
   ((eq? (car expr) '#%DEFINE-VALUES)
    (let ((new-vars (append (cadr expr) vars)))
      (for-each (lambda (subexpr)
		  (check-variables* master-expr subexpr new-vars))
		(cddr expr))))
   ((eq? (car expr) '#%STRUCT) 
    (values))
   (else ;; a procedure call
    (for-each (lambda (subexpr)
		(check-variables* master-expr subexpr vars))
	      expr))))

(define (check-variables expr)
  (check-variables* expr (expand-defmacro expr) '()))


(define (file->sexprs filename)
  (let ((sport (open-output-string)))
    (display "(" sport)
    (let ((iport (open-input-file filename)))
      (do ( (line (read-line iport) (read-line iport)) )
	  ((eq? line eof))
	(display line sport)
	(newline sport)))
    (display ")" sport)
    (read-from-string (get-output-string sport))))

(define (check-variables-in-file file)
  (display-all ";; Checking variables in: " file)
  (for-each check-variables (file->sexprs file)))

(define (check-variables-in-files filelist)
  (for-each check-variables-in-file filelist))