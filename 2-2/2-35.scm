(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (fold-right +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

(define x
  (cons (list 1 2) (list 3 4)))

(count-leaves (list x x))
