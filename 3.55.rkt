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

(define (add-streams s1 s2)
  (stream-maps + s1 s2))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (mul-streams s1 s2)
  (stream-maps * s1 s2))

(define (partial-sums s) 
  (stream-cons (stream-first s)
     (add-streams (stream-rest s) (partial-sums s))))

(stream-ref (partial-sums integers) 0)

