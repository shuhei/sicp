(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items (list)))

(square-list-iter (list 1 2 3 4))
; Because front item goes to the cdr side for each iteration.

(define (square-list-left items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items (list)))

(square-list-left (list 1 2 3 4))
; Because the chain is formed in the car side but list's chain should
; be in the cdr side.
