; Set a symbol as a first element so that an empty table can be mutated.
(define (make-table xs)
  (cons 'table xs))

(define (make-empty-table)
  (make-table ()))

(define (get-table key table)
  (define (iter xs)
    (if (null? (cdr xs))
      false
      (if (equal? key (caadr xs))
        (cdadr xs)
        (iter (cdr xs)))))
  (iter table))

(define (put-table key value table)
  (define (iter xs)
    (if (null? (cdr xs))
      (set-cdr! xs (list (cons key value)))
      (if (equal? key (caadr xs))
        (set-cdr! (cadr xs) value)
        (iter (cdr xs)))))
  (iter table))

; Test
; (define x (make-empty-table))
; (get-table 'hello x)
; (put-table 'hello 123 x)
; (get-table 'hello x)
; (put-table 'hello 234 x)
; (put-table 'world 3 x)
; (get-table 'hello x)
; (get-table 'world x)
