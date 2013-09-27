#lang racket
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
  (define (set-x! v) (set! front-ptr v))
  (define (set-y! v) (set! rear-ptr v))
  (define (dispatch m)
    (cond ((eq? m 'car) front-ptr)
          ((eq? m 'cdr) rear-ptr)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch))


(define (car z) 
  (z 'car))

(define (cdr z) (z 'cdr))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))


(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))


(define (insert-queue! queue item)
  (let ((new-pair (make-queue)))
    (set-car! new-pair item)
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (print-queue queue)
  (let ((new-queue (make-queue)))
    (cond ((empty-queue? queue) (display "\n"))
        (else 
         (display (front-queue queue))
         (display " ")
         (set-front-ptr! new-queue (cdr (front-ptr queue)))
         (print-queue new-queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define q (make-queue))
(car q)
(cdr q)
(empty-queue? q)
(insert-queue! q 'b)
(insert-queue! q 'a)
(insert-queue! q 'c)
(delete-queue! q)
(print-queue q)