(define (fringe xs)
  (cond ((null? xs) xs)
        ((list? xs)
         (append (fringe (car xs))
                 (fringe (cdr xs))))
        (else (list xs))))

(define x
  (list (list 1 2) (list 3 4)))

(fringe x)
(fringe (list x x))
