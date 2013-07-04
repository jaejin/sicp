#lang racket
(define (make-monitored fun)
  (let ((how-many-calls 0))
    (lambda (value)
      (if (eq? 'how-many-calls? value)
          how-many-calls
          (begin (set! how-many-calls (+ how-many-calls 1))
                 (fun value))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 25)
(s 'how-many-calls?)