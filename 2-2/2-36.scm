(define (accumulate-n op init segs)
  (if (null? (car segs))
    ()
    (cons (fold-right op init (map car segs))
          (accumulate-n op init (map cdr segs)))))

(define s
  (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)
