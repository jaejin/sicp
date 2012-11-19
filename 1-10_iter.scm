#lang scheme
(define exercise 1.10 )

(define (f-iter a b c count)
   (if (= count 0)
     c
        (f-iter (+ a (* 2 b) (* 3 c) )
               a   b  (- count 1) ) ) ) 
     
(f-iter 2 1 0 5)