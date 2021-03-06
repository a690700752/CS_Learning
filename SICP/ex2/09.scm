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

(define (width-interval i1) 
  (/ (- (upper-bound i1) (lower-bound i1)) 2.0))

;; add
(define i (make-interval 2 6))
(define j (make-interval 3 10))

(+ (width-interval i)
   (width-interval j))

(width-interval (add-interval i j))

;; mul
(define i1 (make-interval 2 6))
(define i2 (make-interval 3 10))
(* (width-interval i1)
   (width-interval i2))

(width-interval (mul-interval i1 i2))