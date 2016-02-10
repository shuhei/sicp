; Utilities
(define (enumerate-interval from to)
  (if (> from to)
    ()
    (cons from
          (enumerate-interval (+ 1 from) to))))

(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

(define (unique-triples n)
   (flatmap
     (lambda (i)
       (flatmap
         (lambda (j)
            (map (lambda (k) (list i j k))
                 (enumerate-interval 1 (- j 1))))
         (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

; s-sum-triples
(define (s-sum-triples n s)
  (filter (lambda (xs) (= (+ (car xs) (cadr xs) (caddr xs)) s))
          (unique-triples n)))

; Test
(s-sum-triples 7 10)
