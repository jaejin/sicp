#lang scheme
(define (square x) (* x x))
(define (cont-frac n d k plusMinus)
  (define (f n d i k) 
    (cond ((= i k)
          (/ (n i) (d i)))
           (else  
            (/ (n i)
              (plusMinus (d i)  (f n d  (+ i 1)  k)  )
                )
              )
            )
           )
    (f n d 1.0 k)
    )
(define (tan-cf x k) 
    (cont-frac 
           (lambda (i) 
             (cond ((= 1 i) x)
                  (else (square x))))
          (lambda (i)
               (cond 
                 ((= i 1) 1)
                 ((= (remainder i 2) 0) (+ i 1))
                 (else (+ i 2))))
          k - ))

(tan-cf 1 10000) 
