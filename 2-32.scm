#lang racket
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) 
                            (remove-list x s)) rest)))))

;;(define (subsets s)
;;  (if (null? s)
;;      (list '())
;;      (let ((rest (subsets (cdr s))))
;;        (append rest (map (lambda (x)
;;                            (append (list (car s)) x))
;;                          rest)))))

(define (remove-list x list)
  (cond ((or (null? x) (null? list)) list)
        ((list? x) (remove-list (cdr x) (remove (car x) list)))
        (else (remove x list))))

(subsets '(1 2 3))
