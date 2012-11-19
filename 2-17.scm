#lang racket
(define (last-pair list-value)
  (cond ((= (length list-value) 1) (list list-value))
        (else (last-pair (cdr list-value)))))

(car (last-pair (list 23 72 149 34)))
