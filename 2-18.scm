#lang scheme
(define (reverse list-value)
  (cond ((null? list-value) '())
        (else (append (reverse (cdr list-value)) (list (car list-value))))))
(reverse (list 1 4 9 16 25))