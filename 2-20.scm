#lang racket
(define (f x y . z) 
  (display x)
  (newline)
   (display y)
    (newline)
   (display z))

(f 1 2 3 4 5 6)

(define (same-parity first . rest)
  (define (get-list f rest)
    (cond ((null? rest) '())
          ((f (car rest))
           (cons (car rest) (get-list f (cdr rest))))
          (else
           (get-list f (cdr rest)))))
  (if (even? first)
      (cons first (get-list even? rest))
      (cons first (get-list odd? rest))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)