(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (lambda (x)
    (cond ((<= n 1) (f x))
          (else ((compose f (repeated f (- 1 n))) x)))))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/
     (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smoothed f n)
  (repeated (smooth f) n))


