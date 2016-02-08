(define (square x)
  (* x x))

(define (square-list-recur items)
  (if (null? items)
    (list)
    (cons (square (car items))
          (square-list-recur (cdr items)))))

(square-list-recur (list 1 2 3 4))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4))
