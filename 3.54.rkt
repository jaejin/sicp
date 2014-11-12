;; 3.54
#lang racket
(require racket/stream)

(define (stream-maps proc . argstreams)
  (if (stream-empty? (car argstreams))
     '()
     (stream-cons 
      (apply proc (map stream-first argstreams))
      (apply stream-maps
            (cons proc (map stream-rest argstreams))))))

(define fibs
  (stream-cons 0
              (stream-cons 1
                          (add-streams (stream-rest fibs)
                                     fibs))))

(define (add-streams s1 s2)
  (stream-maps + s1 s2))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (mul-streams s1 s2)
  (stream-maps * s1 s2))

(define factorials (stream-cons 1 (mul-streams factorials integers)))

(stream-ref (mul-streams integers integers) 0)

(stream-ref factorials 5)

