#lang scheme
(define (make-segment start-segment end-segment) 
  (cons start-segment (list end-segment)))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (car (cdr p)))

(define (make-point x-point y-point)
  (cons x-point y-point))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (cons (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment)))
           2)
        (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment)))
           2)))
