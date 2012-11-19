#lang scheme
(define (pascal_triangle a b ) 
  (cond 
       ((= a b) 1)
        ((= a 2) 1)
         ((= b 1) 1)
       (else (+ (pascal_triangle (- a 1) (- b 2))
               (pascal_triangle (- a 1) (- b 1))))))


(pascal_triangle 4 4 ) 



