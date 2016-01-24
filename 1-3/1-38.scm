(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (e-fraction k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (if (= (remainder i 3) 2)
                 (* (/ (+ i 1) 3) 2)
                 1))
             k))

(+ 2 (e-fraction 10))
;Value: 2.7182817182817183
(+ 2 (e-fraction 15))
;Value: 2.718281828470584
(+ 2 (e-fraction 20))
;Value: 2.718281828459045
