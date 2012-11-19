#lang scheme
;;Exercise 1.34.  Suppose we define the procedure
(define (square x)
  (* x x))

(define (f g)
  (g 2))

;;Then we have

(f square)
;;4

(f (lambda (z) (* z (+ z 1))))
;;6

;;What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(f f)
;;argument가 f를 받을 수 없어서 에러남 자산이 자신을 또 호출하다보니 arguments는 계속해서 늘어남 . 그래서 에러남 

