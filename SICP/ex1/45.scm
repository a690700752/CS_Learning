#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-guess guess)
    (define next (f guess))
    (println next)
    (if (close-enough? next guess)
        guess
        (try-guess next)))

  (println first-guess)
  (try-guess first-guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
  )

(define (square n) (* n n))

(define (pow base n)
  (cond ((= n 0) 1)
        ((even? n) (pow (square base) (/ n 2)))
        (else (* base (pow base (- n 1)))))
  )

(define (power-fixed-f c n)
  (lambda (x)
    (/ c (pow x (- n 1)))))

(define (n-square-root c npow nave first-guess)
  (fixed-point ((repeated average-damp nave) (power-fixed-f c npow)) first-guess))

;; (define (println n)
;;   (display n)
;;   (newline))

(define (println n)
  #t)

(n-square-root 16 4 2 1.0)
