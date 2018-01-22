#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (divides? a b)
  (= 0 (remainder a b)))

(define (divide-count a b)
  (if (divides? a b)
      (+ 1 (divide-count (/ a b) b))
      0))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car a)
  (divide-count a 2))

(define (cdr a)
  (divide-count a 3))

(car (cons 8 7))
(cdr (cons 8 7))
