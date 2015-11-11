(define (square x)
        (* x x))

(define (square-sum x y)
        (+ (square x) (square y)))

(define (of-larger-two f x y z)
        (cond ((and (<= x y) (<= x z)) (f y z))
              ((and (<= y x) (<= y z)) (f x z))
              (else (f x y))))

(define (square-sum-of-larger-two x y z)
        (of-larger-two square-sum x y z))
