(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))

((double inc) 1)
;Value: 3

(((double (double double)) inc) 5)
;Value: 21

; Substitutions
(((double (lambda (f) (double (double f)))) inc) 5)
(((lambda (x)
    ((lambda (x) (double (double x)))
     ((lambda (x) (double (double x))) x)))
  inc) 5)
(((lambda (x) (double (double x)))
  ((lambda (x) (double (double x))) inc))
 5)
(((lambda (x) (double (double x)))
  (double (lambda (x) (inc (inc x)))))
 5)
; Not the exact substitution but to simplify...
(((lambda (x) (double (double x)))
  (lambda (x) (inc (inc (inc (inc x)))))
 5)
((double (double (lambda (x) (inc (inc (inc (inc x)))))))
 5)
; Finally 2^4 `inc`s!
