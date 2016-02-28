(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersect-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         ())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersect-set (cdr set1) set2)))
        (else (intersect-set (cdr set1) set2))))

(define (union-set s1 s2)
  (if (null? s1)
    s2
    (union-set (cdr s1)
               (adjoin-set (car s1) s2))))

; Test
(element-of-set? 3 (list 1 2 4 8 9))
(element-of-set? 4 (list 1 2 4 8 9))

(adjoin-set 3 (list 1 2 3 4 5))
(adjoin-set 9 (list 1 2 3 4 5))

(intersect-set (list 1 2 3) (list 3 2 5))
(intersect-set () (list 3 2 5))

(union-set (list 1 2 3) (list 3 2 5))
(union-set (list 1 2 3) (list 4 5))
