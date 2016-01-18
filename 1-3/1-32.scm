; 1. Iterative process.
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; 2. Recursive process.
(define (accumulate combiner null-value term a next b)
  (define (iter a)
    (if (> a b)
      null-value
      (combiner (term a) (iter (next a)))))
  (iter a))
