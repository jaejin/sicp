#lang scheme
(define (for-each function list-value)
  (cond ((null? list-value) (newline))
        (else 
         (function (car list-value))
         (for-each function (cdr list-value)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))