#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons a b))
(car (lambda (m) (m a b)))
((lambda (m) (m a b)) (lambda (p q) p))
((lambda (p q) p) a b)
(lambda (a b) a)
a
