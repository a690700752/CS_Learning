#lang planet neil/sicp

;; f(x) = 1 + 1 / x
;; x = 1 + 1 / x
;; x^2 = x + 1

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

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)
