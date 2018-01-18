#lang planet neil/sicp

;; a
(define (cont-frac n d k)

  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))

  (rec 0))

(define (test k)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))

(test 10)
(test 12)
(test 13)
(test 15)
(test 20)

;; b
(define (cont-frac2 n d k)
  (define (iter i ans)
    (if (= i -1)
        ans
        (iter (- i 1) (/ (n i) (+ (d i) ans)))))
  (iter (- k 1) 0))

(define (test2 k)
  (/ 1 (cont-frac2 (lambda (i) 1.0)
                   (lambda (i) 1.0)
                   k)))

(newline)
(test2 10)
(test2 12)
(test2 13)
(test2 15)
(test2 20)
