; Good for cases that have a lot of adjoin and union,
; and a fewer search and intersect.

; O(nd) where d is duplication count
; d times slower in the worst case than non-duplicate version.
; Not bad for checking frequently inserted items.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; O(1)
; n times faster than non-duplicate version.
(define (adjoin-set x set)
  (cons x set))

; O(mnd^2) where d is duplication count
; d^2 times slower than non-duplicate one.
(define (intersect-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         ())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersect-set (cdr set1) set2)))
        (else (intersect-set (cdr set1) set2))))

; O(m) where s1 has m items and s2 has n items
; Faster than non-duplicate one.
(define (union-set s1 s2)
  (append s1 s2))

; Test
(element-of-set? 3 (list 1 2 4 8 9))
(element-of-set? 4 (list 1 2 4 8 9))

(adjoin-set 3 (list 1 2 3 4 5))
(adjoin-set 9 (list 1 2 3 4 5))

(intersect-set (list 1 2 3) (list 3 2 5))
(intersect-set () (list 3 2 5))

(union-set (list 1 2 3) (list 3 2 5))
(union-set (list 1 2 3) (list 4 5))
