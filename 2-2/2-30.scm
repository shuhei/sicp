(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else
          (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(define (square-tree-map tree)
  (map (lambda (x)
         (if (list? x)
           (square-tree-map x)
           (* x x)))
       tree))

(square-tree-map
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
