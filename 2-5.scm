(define (expt-pair a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (findpow value a)
  (define (findpow-sub c d pow)
    (cond ((= (remainder c d) 0)
          (findpow-sub (/ c d) d (+ pow 1)))
          (else pow)))
  (findpow-sub value a 0))

(define (car a)
  (findpow a 2))

(define (cdr a)
  (findpow a 3))

