(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (coefficient k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 4)
          (else 2)))
  (define (add k acc)
    (if (< k 0)
      acc
      (add (- k 1) (+ (* (coefficient k) (y k)) acc))))
  (/ (* h
        (add n 0))
     3.0))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
;Value: .24998750000000042
(integral cube 0 1 0.001)
;Value: .249999875000001

(simpson cube 0 1 100)
;Value: .24671666666666667
(simpson cube 0 1 1000)
;Value: .24966716666666666
