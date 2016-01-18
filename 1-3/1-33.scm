; 1. Iterative process.
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a)
            (if (filter a)
              (combiner result (term a))
              result))))
  (iter a null-value))

(define (prime-squares-sum a b)
  (define (inc x) (+ x 1))
  (define (square x) (* x x))
  (filtered-accumulate prime? + 0 square a inc b))

; 2. Recursive process.
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a)
    (if (> a b)
      null-value
      (if (filter a)
        (combiner (term a) (iter (next a)))
        (iter (next a)))))
  (iter a))
