#lang planet neil/sicp

(define (square n)
  (* n n))

(define (expmod base e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod base (/ e 2) m))
                    m))
        (else (remainder (* base (expmod base (- e 1) m))
              m))))

(define (fermat-test n a)
  (= a (expmod a n n)))

(define (fermat-prime? n)
  (carmichael-iter n 1))

(define (carmichael-iter n a)
  (cond ((= n a) #t)
        ((fermat-test n a)
         (carmichael-iter n (+ a 1)))
        (else #f)))

(fermat-prime? 17)
(fermat-prime? 16)
(fermat-prime? 15)
(fermat-prime? 14)
(fermat-prime? 13)
(newline)
(fermat-prime? 561)
(fermat-prime? 1105)
(fermat-prime? 1729)
(fermat-prime? 2465)
(fermat-prime? 2821)
(fermat-prime? 6601)
