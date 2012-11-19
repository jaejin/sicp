(define (square x)
  (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess x)
    (<  (abs (- (square guess)  x) ) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (let ((next (improve guess x)))
      (if (good-enough? guess next)
        next
        (sqrt-iter next)))
      )
  (sqrt-iter 1.0))

(define (good-enough? guess x)
    (<  (abs (- (square guess)  x) ) 0.001))

(define (improve guess x)
    (average guess (/ x guess)))

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try next guess)
    (if (close-enough? guess next)
          next
          (try (f guess) guess)))
  (try (f first-guess) first-guess))

(define (iterator-improve2 close-enough? try guess first-guess)
  (if (close-enough? v1 v2)
      v2
      (iterator-improve2 (try (f ) v2) )
      ))

(define (iterator-improve enough improve guess x)
    (if (enough guess x)
        guess
        (iterator-improve enough improve  (improve guess x) x)))
