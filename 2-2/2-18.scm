(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
      acc
      (iter (cdr xs) (cons (car xs) acc))))
  (iter xs (list)))

(reverse (list 1 4 9 16 25))
