(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i acc)
    (if (= i 1)
      acc
      (iter (- i 1) (compose f acc))))
  (iter n f))

((repeated square 2) 5)
;Value: 625
((repeated square 3) 5)
;Value: 390625
