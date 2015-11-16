(define (average x y)
        (/ (+ x y) 2))


; Original implementation.
(define (original-sqrt x)
        (define (good-enough? guess)
                (< (abs (- (square guess) x)) 0.001))
        (define (improve guess)
                (average guess (/ x guess)))
        (define (sqrt-iter guess)
                (if (good-enough? guess)
                    guess
                    (sqrt-iter (improve guess))))
        (sqrt-iter 1.0))

(original-sqrt 2)
; 1.4142156862745097
(original-sqrt 20000000000000000)
; Doesn't converge.
(original-sqrt 0.00000000000002)
; .03125000000021312


; Improved implementation.
(define (sqrt x)
        (define (improve-rate guess)
                (/ (- (improve guess) guess) guess))
        (define (good-enough? guess)
                (< (abs (improve-rate guess)) 0.0000001))
        (define (improve guess)
                (average guess (/ x guess)))
        (define (sqrt-iter guess)
                (if (good-enough? guess)
                    guess
                    (sqrt-iter (improve guess))))
        (sqrt-iter 1.0))

(sqrt 2)
; 1.4142135623746899
(sqrt 20000000000000000)
; 141421356.23732778
(sqrt 0.00000000000002)
; 1.4142135785200824e-7
