;; Author: Isaac H. Lopez Diaz
;; Description: Store module (DAG)

;; Nodes
(define (make-node id op inputs)
  (cons (cons 'node id)
	(cons op inputs)))

(define (node-id node)
  (cdr (car node)))

(define (node-op node)
  (car (cdr node)))

(define (node-inputs node)
  (cdr (cdr node)))

(define (set-node-id! node id)
  (set-cdr! (car node) id))

(define (set-node-op! node op)
  (set-cdr! (car node) op))

(define (set-node-inputs! node inputs)
  (set-cdr! (cdr node) inputs))

(define (is-node? node-or-edge)
  (eq? 'node (car (car node-or-edge))))

;; store/dag operations
