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

;;(define (product a b)
;;  (define (next a)
;;    (+ a 1)
;;    )
;;  (define (term a)
;;    (/ 
;;      (* (* a 2) (* a 2))
;;      (* (- (* a 2) 1) (+ (* a 2) 1)))
;;    )
;;  (* (product term a next b))
;;  )

;;(define (product-iter a b)
;;  (define (next a)
;;    (+ a 1)
;;    )
;;  (define (term a)
;;    (/ 
;;      (* (* a 2) (* a 2))
;;      (* (- (* a 2) 1) (+ (* a 2) 1)))
;;    )
;;  (* (product-iter term a next b))
;;  )

;;(product-iter 1.0 10000)
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (inc-even a)
         (if (odd? a)
             (+ a 1)
             (+ a 2)))

(define (inc-odd a)
          (if (odd? a)
             (+ a 2)
             (+ a 1)))


(define (factorial term a)
  (define (next a)
    (+ a 1))
  (product term 1 next a))

(define (pi-product a b)
  (define (next a)
    (+ a 1))
  (define (inc-even a)
         (if (odd? a)
             (+ a 1)
             (+ a 2)))
  (define (inc-odd a)
    (if (odd? a)
        (+ a 2)
        (+ a 1)))
  (* (/ (product inc-even a next b) 
     (product inc-odd a next b)) 4.0)
)

(define (pi-product-iter a b)
  (define (next a)
    (+ a 1))
  (define (inc-even a)
         (if (odd? a)
             (+ a 1)
             (+ a 2)))
  (define (inc-odd a)
    (if (odd? a)
        (+ a 2)
        (+ a 1)))
  (* (/ (product-iter inc-even a next b) 
     (product-iter inc-odd a next b)) 4.0)
)
;;(pi-product 1 1000)
(pi-product-iter 1 10000)
;;(* 8 (pi-sum 1 1000))
