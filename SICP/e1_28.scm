#lang planet neil/sicp

(define (square x) (* x x))

(define (check-expmod n in out)
  (cond ((= in 1) out)
        ((= in (- n 1)) out)
        ((= out 1) 0)
        (else out)))

(define (squaremod in m)
  (check-expmod m in (remainder (square in) m)))

(define (expmod a e m)
  (cond ((= 0 e) 1)
        ((even? e) (squaremod (expmod a (/ e 2) m) m))
        (else (remainder (* a (expmod a (- e 1) m)) m))))

(define (fermat-test n)
  (= 1 (expmod (+ 1 (random (- n 2)))
               (- n 1)
               n)))

(define (fermat-test-withtimes n times)
  (cond ((= 0 times) #t)
        ((fermat-test n) (fermat-test-withtimes n (- times 1)))
        (else #f)))

(define (prime? n)
  (fermat-test-withtimes n 100))

(define (check a b)
  (if (eq? a b)
      (display "true")
      (display "false"))
  (newline))

(check (prime? 17) #t)
(check (prime? 16) #f)
(check (prime? 15) #f)
(check (prime? 14) #f)
(check (prime? 13) #t)
(check (prime? 561) #f)
(check (prime? 1105) #f)
(check (prime? 1729) #f)
(check (prime? 2465) #f)
(check (prime? 2821) #f)
(check (prime? 6601) #f)
