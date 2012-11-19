#lang scheme
(define (make-rectagle a b) 
  (cons a (list a)))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (car (cdr p)))

(define (width p)
  (- (x-point (end-segment p))
     (x-point (start-segment p))))

(define (height p)
  (- (y-point (end-segment p))
     (y-point (start-segment p))))

(define (perimeter rectangle)
  (*  2 
      (+ (width rectangle) (height rectangle))))

(define (area rectangle)
  (*  (width rectangle) (height rectangle)))

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
