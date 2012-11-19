#lang scheme
(define (d i)
  (let ((rem (remainder i 3)))
    (if (or (= rem 0) (= rem 1))
        1
        (* 2 (+ 1 (quotient i 3))))))


(define (cont-frac-iter n d k)
   (define (f n d i k result)  
    (cond ((= 1 k) 1)
      ((= i k)
          result)
           (else  
             (f n d  (+ i 1)  k 
               (/ (n i)
                 (+ (d i)  result))
               )
             )
             )
        )
    (f n d 1.0 k 1)
    )
(define (approximate-e k) 
(cont-frac-iter (lambda (i) 1.0) d k))

(d 11)
(approximate-e 11)