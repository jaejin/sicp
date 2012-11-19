#lang scheme
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value)
)

(define (term a) a )
(define (next a) (+ a 1 ))
(define (prev a) (- a 1 ))

(define (prime-term a)
  (if (prime? a)
      (square a)
      0 ))


  
(accumulate-iter + 0 prime-term 1 next 10)
(define (product-of-relative-primes n)
    (define (relatively-prime-to-n? a)
    (cond ((= (gcd a n) 1)
        a
        )
        (else 
         1 )
        ))
  (accumulate-iter * 1 relatively-prime-to-n? 1 next n))

(product-of-relative-primes 8)
(product-of-relative-primes 4)
(product-of-relative-primes 9)