#lang scheme
(define (term a) a )
(define (next a) (+ a 1 ))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;다름 
(define (sum-iter2 term a next b)
  (define ( iter a result)
    (if (= a b)
        result
        (iter (next a) ( + (term (next a)) result)))
    )
  (iter a (term a)))

;;정답
(define (sum-iter term a next b)
  (define ( iter a result)
    (if (> a b)
        result
        (iter (next a) ( + (term a) result)))
    )
  (iter a 0))

(sum cube 4 next 5)
(sum-iter cube 4 next 5)
(sum-iter2 cube 4 next 5)
(sum-iter2 cube 1 next 5)
(sum-iter cube 1 next 5)