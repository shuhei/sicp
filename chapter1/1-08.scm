(define (cubic-root x)
        (define (good-enough? previous guess)
                (< (abs (/ (- guess previous) guess)) 0.0000001))
        (define (improve guess)
                (/ (+ (/ x (square guess)) (* 2 guess)) 3))
        (define (cubic-root-iter previous guess)
                (if (good-enough? previous guess)
                    guess
                    (cubic-root-iter guess (improve guess))))
        (cubic-root-iter 2.0 1.0))
