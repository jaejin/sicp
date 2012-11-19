#lang scheme
(define (square a)
  (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next test-divisor)
  (cond ((= test-divisor 2)
           3
         )
       (#t
        (+ test-divisor 2)
        )))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0 ))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
       ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                  m))
       (else 
        (remainder (* base (expmod base (- exp 1) m))
                  m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n ) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
       ((fermat-test n) (fast-prime? n (- times 1)))
       (else false)))

(define (prime? n)
 ( if  (= n (smallest-divisor n)) 
      #t     
      #f
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  )

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (current-inexact-milliseconds) start-time)) #f
      )
  )

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (search-for-primes start end n )
  (cond ((= start end)
        start
    )
       ((= n 3)  (newline))
       (#t
          (if (timed-prime-test start)
              (search-for-primes  (+ 1 start) end  (+ n 1))
              (search-for-primes  (+ 1 start) end  n)
             )
         ))
       )

(search-for-primes 100 1000 0)
(search-for-primes 1000 10000 0)
(search-for-primes 10000 100000 0)