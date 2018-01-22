#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))


