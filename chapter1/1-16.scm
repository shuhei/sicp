(define (even? n)
        (= (remainder n 2) 0))

(define (fast-expr b n)
        (cond ((= n 0) 1)
              ((even? n) (square (fast-expr b (/ n 2))))
              (else (* b (fast-expr b (- n 1))))))

(define (iter-expr b n)
        (define (expr-iter a m)
                (cond ((= m n) a)
                      ((or (= m 0) (> (* m 2) n)) (expr-iter (* b a ) (+ m 1)))
                      (else (expr-iter (square a) (* m 2)))))
        (expr-iter 1 0))
