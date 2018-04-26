#lang racket
(require racket/trace)
(require (planet neil/sicp))

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))

(define (add-interval i1 i2)
  (make-interval (+ (lower-bound i1) (lower-bound i2))
                 (+ (upper-bound i1) (upper-bound i2))))

(define (sub-interval i1 i2)
  (make-interval (- (lower-bound i1)
                    (upper-bound i2))
                 (- (upper-bound i1)
                    (lower-bound i2))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y))) 
        (p2 (* (upper-bound x) (lower-bound y))) 
        (p3 (* (lower-bound x) (upper-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y))))
   (make-interval (min p1 p2 p3 p4) 
                  (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Error div, span zero." y)
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (mul2-interval x y)
  (define xl (lower-bound x))
  (define xh (upper-bound x))
  (define yl (lower-bound y))
  (define yh (upper-bound y))
  (cond ((and (< xl 0) (< xh 0)) (cond ((and (< yl 0) (< yh 0)) (make-interval (* xh yh) (* xl yl)))
                                       ((and (< yl 0) (>= yh 0)) (make-interval (* xl yh) (* xl yl)))
                                       (else (make-interval (* xl yh) (* xh yl)))))
        ((and (< xl 0) (>= xh 0)) (cond ((and (< yl 0) (< yh 0)) (make-interval (* xh yl) (* xl yl)))
                                        ((and (< yl 0) (>= yh 0)) (make-interval (min (* xl yh) (* xh yl)) (max (* xh yh) (* xl yl))))
                                        (else (make-interval (* xl yh) (* xh yh)))))
        (else (cond ((and (< yl 0) (< yh 0)) (make-interval (* xh yl) (* xl yh)))
                    ((and (< yl 0) (>= yh 0)) (make-interval (* xh yl) (* xh yh)))
                    (else (make-interval (* xl yl) (* xh yh)))))))

(define (width-interval i1) 
  (/ (- (upper-bound i1) (lower-bound i1)) 2.0))

(define (eq-interval? x y)
  (and (= (lower-bound x) (lower-bound y)) 
       (= (upper-bound x) (upper-bound y))))

(define (make-center-percent c p) 
  (define w (* c p))
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (percent i)
  (/ (width-interval i) (center i)))

(make-center-percent 10 0.5)
(center (make-interval 5 20))
(center (make-center-percent 10 0.0001))
(percent (make-center-percent 10 0.0001))


