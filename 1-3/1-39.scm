(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                 x
                 (* x x -1)))
             (lambda (i)
               (- (* 2 i) 1))
             k))

(tan 1.0)
;Value: 1.557407724654902
(tan-cf 1.0 10)
;Value: 1.557407724654902
(tan-cf 1.0 15)
;Value: 1.557407724654902
(tan-cf 1.0 20)
;Value: 1.557407724654902
