; Exercise 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

; Exercise 2.54
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

; Exercise 2.55
; 'something creates a special form (quote something) which is a symbol called something.
; ''something creates (quote (quote someting)) which is a list of quote and a symbol called something.
