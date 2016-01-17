(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc x) (+ x 1))

(define (identity x) (x))

(define (factorial a b)
  (product identity a inc b))

(define (compute-pi n)
  (define (a k) (+ 2.0 (* 2.0 (quotient k 2))))
  (define (b k) (+ 1.0 (* 2.0 (quotient (+ k 1) 2))))
  (define (term k) (/ (a k) (b k)))
  (* (product term 1 inc n)
     4.0))

; Test
(compute-pi 1000)
(compute-pi 10000)
(compute-pi 100000)
