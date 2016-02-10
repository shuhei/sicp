(define (reverse sequence)
  (fold-right (lambda (x acc) (append acc (list x)))
              ()
              sequence))

(reverse (list 1 2 3))

(define (reverse sequence)
  (fold-left (lambda (acc x) (cons x acc))
             ()
             sequence))

(reverse (list 1 2 3))
