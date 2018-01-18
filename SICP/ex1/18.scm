(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (my2* a b)
  (my2*-iter a b 0))

(define (my2*-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (my2*-iter (double a) (halve b) product))
        (else (my2*-iter a (- b 1) (+ product a)))))

(my2* 21 8)

