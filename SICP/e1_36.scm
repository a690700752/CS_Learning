#lang planet neil/sicp

(define (println n)
  (display n)
  (newline))

(define tolerance 0.00001)
(define (fixed-point f first-guess)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try-guess guess)
    (let ((next (f guess)))
      (println next)
      (if (close-enough? next guess)
          next
          (try-guess next))))

  (println first-guess)
  (try-guess first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)

(newline)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
             2.0)
