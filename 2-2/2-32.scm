(define (subsets s)
  (if (null? s)
    (list ())
    (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (rs) (cons (car s) rs))
                   rest)))))

(subsets (list 1 2 3))
