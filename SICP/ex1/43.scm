#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (if (= times 0)
      (lambda (x) x)
      (compose f (repeated f (dec times)))))

(define (square x) (* x x))

((repeated square 2) 5)
