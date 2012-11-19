#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(define y (list x (list x (list 1 2 3))))

(define (count-leaves2 x)
  (accumulate + 0 (map (lambda (x)
                         (if (list? x) (count-leaves2 x) 1)) x)))

(count-leaves x)
(count-leaves y)
(count-leaves2 x)
(count-leaves2 y)
