(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    ()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            (else (intersection-set set1 (cdr set2)))))))

; Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((= x (car set))
         set)
        ((< x (car set))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  (else (cons x2 (union-set set1 (cdr set2)))))))))

; Test
(intersection-set (list 1 2 3) (list 2 3 5 6))
(intersection-set (list 1 2) (list 5 6))

(adjoin-set 3 (list 1 2 4 5))
(adjoin-set 3 (list 1 2 3 5))

(union-set (list 1 2 4) (list 2 3 5))
(union-set () (list 1 2 3))
