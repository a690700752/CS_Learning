#lang planet neil/sicp

(define (product term a next b)
  (if (= a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (= a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (add1 n) (+ n 1))

(define (factorial a)
  (define (term n) n)
  (product term 1 add1 (add1 a)))

(factorial 10)

(define (ceil-even n)
  (+ n (remainder n 2)))

(define (ceil-odd n)
  (if (even? n)
      (add1 n)
      n))

(define (calc-pi n)
  (define (term k)
    (/ (ceil-even k) (ceil-odd k)))
  (* 4 (product-iter term 2 add1 n)))

(exact->inexact (calc-pi 100000))
