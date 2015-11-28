(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

; TODO: Bit-shifting?
(define (halve n)
  (/ n 2))

(define (iter-* a b)
  ; `c + a * b` is the invariant quantity.
  (define (*-iter c a b)
    (cond ((= 0 b) c)
          ((even? b) (*-iter c (double a) (halve b)))
          (else (*-iter (+ c a) a (- b 1)))))
  ; Handle negative b.
  (if (< b 0)
      (iter-* (- a) (- b))
      (*-iter 0 a b)))
