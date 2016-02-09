(define (tree-map proc tree)
  (map (lambda (x)
         (if (list? x)
           (tree-map proc x)
           (proc x)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))
