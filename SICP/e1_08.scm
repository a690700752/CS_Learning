(define (improve_cubrt x y)
  (/ (+ (/ x (* y y))
        (* 2 y))
     3))

(define (good-enough? x y)
  (< (abs (- (improve_cubrt x y)
             y))
     (* 0.001 x)))

(define (newton_cubrt x y)
  (if (good-enough? x y)
      y
      (newton_cubrt x (improve_cubrt x y))))

(define (cubrt x)
  (newton_cubrt x 1.0))


(cubrt 27)
