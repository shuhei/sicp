; (define (* a b)
;         (if (= b 0)
;             0
;             (+ a (* a (- b 1)))))

(define (even? n)
        (= (remainder n 2) 0))

; double and halve are both bit-shifting!
(define (double n)
        (+ n n))

(define (halve n)
        (/ n 2))

(define (fast-* a b)
  (cond ((= b 0) 0)
        ((< b 0) (fast-* (- a) (- b)))
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))
