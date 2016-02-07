(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (calc-factor base n)
  (define (iter num acc)
    (if (= (remainder num base) 0)
      (iter (/ num base) (+ acc 1))
      acc))
  (iter n 0))

(define (car z)
  (calc-factor 2 z))

(define (cdr z)
  (calc-factor 3 z))

(car (cons 5 100))
(cdr (cons 5 100))
