#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))

(define (sub-interval i1 i2)
  (make-interval (- (lower-bound i1)
                    (upper-bound i2))
                 (- (upper-bound i1)
                    (lower-bound i2))))

(sub-interval (make-interval 1 2) 
              (make-interval 2 3))
