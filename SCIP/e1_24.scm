#lang planet neil/sicp
(define (square n)
  (* n n))

(define (expt base e)
  (cond ((= e 0) 1)
        ((even? e) (expt (square base)
                        (/ e 2)))
        (else (* (expt base (- e 1))
                 base))))

(define (expmod base e m)
  (remainder (expt base e) m))

(define (fermat-test n)
  (define (try-it rand)
    (= rand (expmod rand n n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test n) (fast-prime? n (- 1 times)))
        (else false)))

(define (print-prime-time n es-time)
  (display n)
  (display "***")
  (display es-time)
  (newline))

(define (check-print-prime n start-time)
  (if (fast-prime? n 2)
      (print-prime-time n (- (runtime) start-time))
      false))

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