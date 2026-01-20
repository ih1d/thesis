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
(define (b-eval expr env cont str)
  (cond ((
