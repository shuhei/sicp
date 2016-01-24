; Recursive process.
(define (cont-frac n d k)
  (define (iter i)
    (if (= k i)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (calc-frac k)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

(define (converge f tolerance)
  (define (iter i prev)
    (let ((current (f i)))
      (if (< (abs (- current prev)) tolerance)
        i
        (iter (+ i 1) current))))
  ; TODO: Infinite?
  (iter 1 10000000000000.0))

(define (test f tolerance)
  (let ((k (converge f tolerance)))
    (newline)
    (display k)
    (newline)
    (display (f k))))

; 1. Approximation that is accurate to 4 dicimal places.
(test calc-frac 0.0001)
;11
;.6180555555555556

; Test:
(calc-frac 10)
;Value: .6179775280898876
(calc-frac 11)
;Value: .6180555555555556
(calc-frac 12)
;Value: .6180257510729613
(calc-frac 13)
;Value: .6180371352785146

; 2. Iterative process.
(define (cont-frac-iter n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (calc-frac-iter k)
  (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k))

(calc-frac-iter 10)
;Value: .6179775280898876
(calc-frac-iter 11)
;Value: .6180555555555556
(calc-frac-iter 12)
;Value: .6180257510729613
(calc-frac-iter 13)
;Value: .6180371352785146
