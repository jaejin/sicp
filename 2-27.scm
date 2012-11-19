#lang racket
(define x (list 1 (list 1 2) 5 (list 3 4 6) 7 ))
(reverse x)

(define (deep-reverse x)
    (cond ((null? x) '())
          ((list? (car x))
           (append (deep-reverse (cdr x)) (list (reverse (car x)))  ))
          (else 
           (append (deep-reverse (cdr x)) (list (car x)) ))))

(deep-reverse x)