; Compute an element of Pascal's triangle.
; - level: one-indexed level of Pascal's triangle.
; - i: one-indexed position of element in a row.
(define (pascal level i)
        (cond ((< i 1) 0)
              ((> i level) 0)
              ((<= level 1) 1)
              (else (+ (pascal (- level 1) (- i 1)) (pascal (- level 1) i)))))
