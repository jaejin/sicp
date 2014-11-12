#lang racket
;; 3.56

(require racket/stream)

(define (stream-maps proc . argstreams)
  (if (stream-empty? (car argstreams))
     '()
     (stream-cons 
      (apply proc (map stream-first argstreams))
      (apply stream-maps
            (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2)
  (stream-maps + s1 s2))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (mul-streams s1 s2)
  (stream-maps * s1 s2))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
       ((stream-empty? s2) s1)
       (else 
        (let ((s1car (stream-first s1))
              (s2car (stream-first s2)))
          (cond ((< s1car s2car)
                 (stream-cons s1car (merge (stream-rest s1) s2)))
               ((> s1car s2car)
                (stream-cons s2car (merge s1 (stream-rest s2))))
                (else
                 (stream-cons s1car 
                             (merge (stream-rest s1)
                                   (stream-rest s2)))))))))

              


;;http://web.cecs.pdx.edu/~black/AdvancedProgramming/Lectures/Hamming%20Closures/Hamming-6up.pdf

(define (integer-starting-from2 n)
  (stream-cons n (integer-starting-from2 (+ n 2))))

(define (integer-starting-from3 n)
  (stream-cons n (integer-starting-from3 (+ n 3))))

(define (integer-starting-from5 n)
  (stream-cons n (integer-starting-from5 (+ n 5))))


(define (scale-stream n s)
  (stream-cons n (scale-stream (+ n s) s)))


(define S (stream-cons 1 (merge (scale-stream 2 2) (merge (scale-stream 3 3) (scale-stream 5 5)))))

(stream-ref (scale-stream 3 3) 0)
(stream-ref S 8)