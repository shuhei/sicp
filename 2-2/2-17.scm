(define (last-pair xs)
  (if (null? (cdr xs))
    xs
    (last-pair (cdr xs))))

(last-pair (list 23 72 149 34))
