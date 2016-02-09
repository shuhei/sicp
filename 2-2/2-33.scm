; Works as fold-right.
; fold-right always uses `initial` while reduce-right uses it only for nil.
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op
                    initial
                    (cdr sequence)))))

(accumulate + 3 (list 1 2 3))

; map
(define (map p sequence)
  (accumulate (lambda (x acc) (cons (p x) acc))
              ()
              sequence))

(map square (list 1 2 3))

; append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

; length
(define (length sequence)
  (accumulate (lambda (x acc) (+ acc 1)) 0 sequence))

(length (list 1 2 3 4 5))
