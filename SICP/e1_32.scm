#lang planet neil/sicp

(define (accumulate2 combiner null-value term a next b)
  (if (= a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter combiner term a result)
    (if (= a b)
        result
        (iter combiner term (next a) (combiner result (term a)))))
  (iter combiner term a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

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
  (* 4 (product term 2 add1 n)))

(exact->inexact (calc-pi 10000))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-add a b)
  (define (term x) x)
  (sum term a add1 b))

(sum-add 0 10)
