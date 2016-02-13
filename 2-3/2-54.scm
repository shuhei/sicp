(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else
          (eq? a b))))

(equal? '(this is a list)
        '(this is a list))

(equal? '(this is a list)
        '(this (is a) list))
