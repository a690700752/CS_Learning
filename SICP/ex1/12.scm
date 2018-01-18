(define (yanghui row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        (else (+ (yanghui (- row 1) (- col 1))
                 (yanghui (- row 1) col)))))

(yanghui 3 1)
