#lang planet neil/sicp

(define (sum term a next b)
  (if (= a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-add a b)
  (define (term a) a)
  (define (next a) (+ a 1))
  (sum term a next b))

(define (ceil-even n)
  (+ n (remainder n 2)))

(define (add1 n) (+ 1 n))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define even-len (ceil-even n))
  (define (simpson-terma k)
    (define y (f (+ a (* k h))))
    (cond ((or (= k 0) (= k even-len)) y)
          ((even? k) (* 2 y))
          (else (* 4 y))))
  (* (sum simpson-terma 0 add1 (add1 n)) (/ h 3)))

(define (cube x) (* x x x))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)
