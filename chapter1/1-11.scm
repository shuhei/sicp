; Recursive process
(define (f n)
        (cond ((< n 3) n)
              (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

; Iterative process
(define (f n)
        (define (f-iter a b c count)
                (if (= count 0)
                    c
                    (f-iter (+ a b c) a b (- count 1))))
        (f-iter 2 1 0 n))
