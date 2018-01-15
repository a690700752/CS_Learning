(define (expt b n)
  (expt_iter b n 1))

(define (square n) (* n n))

(define (expt_iter b n product)
  (cond ((= n 1) b)
        ((even? n) (expt_iter b (/ n 2) (* (square b) product)))
        (else (expt_iter b (- n 1) (* b product)))))

(define (even? n)
  (= (remainder n 2) 0))

(expt 8 3)
