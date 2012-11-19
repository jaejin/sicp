#lang scheme
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product  term a next b)
  (if (> a b)
      1
      (* (term a )
         (product  term (next a) next b))))

(define (product-iter  term a next b)
  (define ( iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result)))
    )
  (iter a 1))

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a )
                  (accumulate  combiner null-value  term (next a) next b)))  
)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value)
)

(define (term a) a )
(define (next a) (+ a 1 ))

(accumulate * 1 term 1 next 5)
(product-iter  term 1 next 5)
(accumulate-iter * 1 term 1 next 5)
(accumulate-iter + 0 term 1 next 5)