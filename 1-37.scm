#lang scheme
;; a
(define (cont-frac n d k)
  (define (f n d i k) 
    (cond ((= i k)
          (/ (n i) (d i)))
           (else  
            (/ (n i)
              (+ (d i)  (f n d  (+ i 1)  k)  )
                )
              )
            )
           )
    (f n d 1.0 k)
    )

;; b
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
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0)
         11)

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0)
        11)
