#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
      (define next (improve guess))
      (if (good-enough? next guess)
          next
          (iter next)))
    (iter first-guess)))

(define (good-enough? v1 v2)
  (define tolerance 0.00001)
  (< (abs (- v1 v2)) tolerance))

(define (sqrt x)
  ((iterative-improve good-enough?
                     (lambda (guess)
                       (/ (+ guess (/ x guess)) 2))) 1.0))

(sqrt 2)

(define (fixed-point f first-guess)
  ((iterative-improve good-enough? f) first-guess))

(fixed-point cos 1.0)
