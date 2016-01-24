(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (square guess) x)) 0.00001))
                      (lambda (guess)
                        (average guess (/ x guess))))
  1.0))

(sqrt 2.0)
;Value: 1.4142156862745097
(sqrt 9.0)
;Value: 3.000000001396984

(define (fixed-point f)
  (iterative-improve (lambda (guess)
                       (< (abs (- guess (f guess))) 0.00001))
                     (lambda (guess)
                       (f guess))))

((fixed-point cos) 1.0)
;Value: .7390893414033928
