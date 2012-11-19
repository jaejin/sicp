(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2 ))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (lambda (x)
    (cond ((<= n 1) (f x))
          (else ((compose f (repeated f (- 1 n))) x)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x)))
  )

 (define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0 ) )

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
              1.0
              ) )

(define (four-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y))))) 
   1.0))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (n-root  x n )
  (fixed-point (cond ((>= n 4) ((repeated average-damp (- n 2))
                                (lambda (y) (/ x (expt y (- n 1))))))
        (else (average-damp (lambda (y) (/ x (expt y (- n 1)))))))
   1.0))

(define (five-root x)
  (fixed-point (average-damp (average-damp (average-damp (lambda (y) (/ x (* y y y y))))) ) 
   1.0))

