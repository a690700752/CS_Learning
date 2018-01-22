#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
  )

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
