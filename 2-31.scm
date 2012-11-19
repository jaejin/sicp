#lang racket
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (square x) (* x x))

(define (tree-map square tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map square sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
