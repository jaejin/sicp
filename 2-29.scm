#lang scheme



(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define mobile-x (make-mobile (make-branch 20 10) (make-branch 30 40)))

;; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))



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