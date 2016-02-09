(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
      acc
      (iter (cdr xs) (cons (car xs) acc))))
  (iter xs (list)))

(define (deep-reverse xs)
  (define (iter xs acc)
    (cond ((null? xs)
           acc)
          ((list? xs)
           (iter (cdr xs)
                 (cons (iter (car xs) (list)) acc)))
          (else xs)))
  (iter xs (list)))

(define x
  (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)

(define y
  (list (list (list 1 2)
              (list 3 4))
        (list (list 5 6)
              (list 7 8)
              (list 9 10))))

(reverse y)
(deep-reverse y)
