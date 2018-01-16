#lang planet neil/sicp

(define (square n) (* n n))

(define (cont-frac n d k)

  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))

  (rec 0))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 0)
                   x
                   (- (square x))))
             (lambda (i)
               (+ (* 2 i) 1.0))
             k))

(tan-cf 1 20)
(tan-cf 2 20)
(tan-cf 3 20)
(tan-cf 4 20)
(tan-cf 5 20)
