#lang planet neil/sicp
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n t)
  (cond ((> (square t) n) n)
        ((divides? n t) t)
        (else (find-divisor n (+ t 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)