(define (f g) (g 2))

(define (square x) (* x x))

(f square)
;Value: 4

(f (lambda (z) (* z (+ z 1))))
;Value: 6

(f f)
;(f 2)
;(2 2)
;The object 2 is not applicable.
