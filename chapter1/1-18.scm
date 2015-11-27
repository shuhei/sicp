(define (even? n)
        (= (remainder n 2) 0))

(define (double n)
        (+ n n))

(define (iter-* a b)
  (define (*-iter c m)
          (cond ((= m b) c)
                ((or (= m 0) (> (double m) b)) (*-iter (+ a c) (+ m 1)))
                (else (*-iter (double c) (double m)))))
  (if (< b 0)
      (iter-* (- a) (- b))
      (*-iter 0 0)))
