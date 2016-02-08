(define (same-parity c . rest)
  (cons c
        (if (even? c)
          (filter even? rest)
          (filter odd? rest))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
