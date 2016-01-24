(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i acc)
    (if (= i 1)
      acc
      (iter (- i 1) (compose f acc))))
  (iter n f))

(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
