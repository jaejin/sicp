#lang racket

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
        
(define (estimate-integral trials p x1 x2 y1 y2)
  (monte-carlo trials (p x1 x2 y1 y2)))


(define (double x)
  (* x x))

(define (P m n l)
  (lambda (x1 x2 y1 y2)
    (lambda ()
      (<= (+ (double (- (random-in-range x1 x2) m)) (double (- (random-in-range y1 y2) n))) (double l)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define circle (P 5 7 3))
(estimate-integral 10000 circle 2 8 4 10)