(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))

; Exercise 2.63
; 2. Θ(n log n) by the master method
; T(n) = 2T(n/2) + Θ(n) // merge by append, which is Θ(n)
(define (tree->list-1 tree)
  (if (null? tree)
    ()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

; 2. Θ(n) by the master method
; T(n) = 2T(n/2) + Θ(1) // merge by cons, whici is Θ(1)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree ()))

; 1. Same result.
(define tree1
  (list 7
        (list 3 (list 1 () ()) (list 5 () ()))
        (list 9 () (list 11 () ()))))

(define tree2
  (list 3
        (list 1 () ())
        (list 7
              (list 5 () ())
              (list 9 () (list 11 () ())))))

(define tree3
  (list 5
        (list 3 (list 1 () ()) ())
        (list 9 (list 7 () ()) (list 11 () ()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons () elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts
                                       left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))
; 1. It calculates the counts for a left subtree and a right subtree,
; and recursively make the subtrees with the counts.
; It also avoids Θ(n) to get the entry and the head of the right subtree
; taking advantage of remaining elements.

(list->tree (list 1 3 5 7 9 11))
;     5
;  1     9
;   3   7 11

; 2. Θ(n) by the master method
; T(n) = 2T(n/2) + Θ(1) // merge is make-tree, which is Θ(1)

; Exercise 2.65
; 1. Convert to lists: Θ(n)
; 2. Union/intersect lists: Θ(n)
; 3. Make tree from the merged list: Θ(n)
(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else
            (let ((x1 (car list1))
                  (x2 (car list2)))
              (cond ((= x1 x2)
                     (cons x1 (union-list (cdr list1) (cdr list2))))
                    ((< x1 x2)
                     (cons x1 (union-list (cdr list1) list2)))
                    (else
                      (cons x2 (union-list list1 (cdr list2)))))))))
  (list->tree (union-list (tree->list-2 set1)
                          (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (intersection-list list1 list2)
    (if (or (null? list1) (null? list2))
      ()
      (let ((x1 (car list1))
            (x2 (car list2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list (cdr list1) (cdr list2))))
              ((< x1 x2)
               (intersection-list (cdr list1) list2))
              (else
                (intersection-list list1 (cdr list2)))))))
  (list->tree (intersection-list (tree->list-2 set1)
                                 (tree->list-2 set2))))

; Test
(intersection-set (list->tree (list 1 2 3))
                  (list->tree (list 2 3 5 6)))
(intersection-set (list->tree (list 1 2))
                  (list->tree (list 5 6)))

(union-set (list->tree (list 1 2 4))
           (list->tree (list 2 3 5)))
(union-set (list->tree ())
           (list->tree (list 1 2 3)))
