(define (max a b)
  (if (> a b) a b))

(define (squre_sum a b)
  (+ (* a a) (* b b)))

(define (add_two_bigger a b c)
  (if (= a (max a b))
      (squre_sum a (max b c))
      (squre_sum b (max a c))))

(add_two_bigger 4 3 3)
