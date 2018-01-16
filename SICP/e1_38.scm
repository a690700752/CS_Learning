#lang planet neil/sicp

;; a
(define (cont-frac n d k)

  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))

  (rec 0))

(define (test k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (if (= 0 (remainder (- i 1) 3))
                        (* 2 (+ 1 (/ (- i 1) 3)))
                        1))
                  k)))

(test 10)
(test 12)
(test 13)
(test 15)
(test 20)

