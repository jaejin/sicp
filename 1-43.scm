#lang scheme
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (lambda (x)
    (cond ((<= n 1) (f x))
	  (else ((compose f (repeated f (- 1 n))) x)))))

((repeated square 2) 2)

    