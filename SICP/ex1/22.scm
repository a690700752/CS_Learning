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

(define (prime? n)
  (= (smallest-divisor n) n))

(define (print-prime-time n es-time)
  (display n)
  (display "***")
  (display es-time)
  (newline))

(define (check-print-prime n start-time)
  (if (prime? n)
      (print-prime-time n (- (runtime) start-time))
      #f))

(define (iter-check-prime n count)
  (cond ((= count 0) n)
        ((and (not (even? n)) (check-print-prime n (runtime)))
         (iter-check-prime (+ n 1) (- count 1)))
        (else (iter-check-prime (+ n 1) count))))

(define (search-for-primes start count)
  (iter-check-prime start count))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
(search-for-primes 1000000000 3)
(search-for-primes 10000000000 3)
(search-for-primes 100000000000 3)
(search-for-primes 1000000000000 3)
(search-for-primes 10000000000000 3)
(search-for-primes 100000000000000 3)
