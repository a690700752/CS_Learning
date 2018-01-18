#lang planet neil/sicp
(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder a b) 0))

(define (add1 n) (+ n 1))

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n t)
      (cond ((> (square t) n) n)
            ((divides? n t) t)
            (else (find-divisor n (+ t 1)))))

    (find-divisor n 2))
  (= (smallest-divisor n) n))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter combiner term a result)
    (cond ((= a b) result)
          ((filter a) (iter combiner term (next a) (combiner result (term a))))
          (else (iter combiner term (next a) result))))
  (iter combiner term a null-value))

;; a

(define (prime-sum a b)
  (define (term n) n)
  (filtered-accumulate + 0 term a add1 b prime?))

(prime-sum 5 10) ; 1 2 3 5 7 = 18

;; b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (less-primes-product n)
  (define (term n) n)
  (define (multual-prime? a)
    (= 1 (gcd n a)))
  (filtered-accumulate * 1 term 1 add1 n multual-prime?))

(less-primes-product 10)
