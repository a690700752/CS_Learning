(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f2 n)
  (f2_iter 2 1 0 n))

(define (f2_iter np1 np2 np3 count)
  (if (= count 0)
      np1
      (f2_iter (+ np1 (* 2 np2) (* 3 np3)) np1 np2 (- count 1))))

(f 19)
(f2 17)

