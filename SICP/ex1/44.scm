#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f times)
  (if (= times 0)
      (lambda (x) x)
      (compose f (repeated f (dec times)))))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (smooth-n-times f n)
  (repeated smooth n) f)
