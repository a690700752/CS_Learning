(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (my* a b)
  (cond ((= b 1) a)
        ((even? b) (my* (double a) (halve b)))
        (else (+ a (my* a (- b 1))))))

(my* 10 7)

