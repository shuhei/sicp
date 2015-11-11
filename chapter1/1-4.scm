; When b is greater than 0, the operator becomes `+`, thus `(+ a b)`.
; When b is less than or equal to 0, the operator becomes `-`, thus (- a b)`.
; The behavior above works exactly same as a sum of a and absolute value of b.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
