;; Author: Isaac H. Lopez Diaz
;; Description: Main module

;; expr
(define (constant? expr)
  (or (number? expr)
      (boolean? expr)))

(define (var? expr)
  (symbol? expr))

(define (if? expr)
  (eq? 'if (car expr)))

;; eval

;; apply

;; repl
(define (blue level turn)
  (lambda (ans)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display ": ")
      (write ans)
      (newline)
      (write level) (write '-) (write (+ 1 turn)) (display " BLUE> ")
      (let ((r (read)))
	(if (eq? r 'switch)
	    ((Mcont '(switch to graph)) (blue level turn))
	    (((blue level (+ turn 1)) r) Mcont))))))

(define (graph level turn)
  (lambda (ans)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display ": ")
      (write ans)
      (newline)
      (write level) (write '-) (write (+ 1 turn)) (display " GRAPH> ")
      (let ((r (read)))
	(if (eq? r 'switch)
	    ((Mcont '(switch to blue)) (graph level turn))
	    (((graph level (+ turn 1)) r) Mcont))))))

;; initialize
(define (init-repls)
  (cons (blue 0 0)
	(graph 0 0)))

(define (main)
  (let* ((repls (init-repls))
	 (blue-repl (car repls))
	 (graph-repl (cdr repls)))
    ((blue-repl 'start) graph-repl)))
