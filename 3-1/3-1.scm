(define (make-accumulator v)
  (lambda (num)
    (set! v (+ v num))
    v))

(begin
  (define A (make-accumulator 5))
  (assert (= (A 10) 15))
  (assert (= (A 10) 25)))
