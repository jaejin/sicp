#lang racket

;;d
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define mobile-x (make-mobile (make-branch 20 10) (make-branch 30 40)))

;; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))



;;b
(define (total-weight mobile)
  (+  (branch-structure (right-branch mobile))
      (branch-structure (left-branch mobile))))

;; c
(define (balanced mobile)
  (= 
   (* (branch-structure (right-branch mobile)) 
      (branch-length (right-branch mobile)) )
  (* (branch-structure (left-branch mobile)) 
      (branch-length (left-branch mobile)) )))

(total-weight mobile-x)