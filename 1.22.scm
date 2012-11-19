#lang scheme
(define (square a)
  (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0 ))

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
  (if (prime? n)
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