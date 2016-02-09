(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; 1. Selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

; 2. Total weight
(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (pair? mobile)
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))
    mobile))

; Test
(total-weight (make-mobile (make-branch 1 12)
                           (make-branch 2
                                        (make-mobile (make-branch 3 4)
                                                     (make-branch 4 3)))))

; 3. Balanced mobile
(define (branch-length branch)
  (car branch))

(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced-mobile? mobile)
  (if (pair? mobile)
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and (balanced-mobile? (branch-structure left))
           (balanced-mobile? (branch-structure right))
           (= (branch-torque left)
              (branch-torque right))))
    true))

; Test: Balanced
(balanced-mobile? (make-mobile (make-branch 3 4)
                               (make-branch 2
                                            (make-mobile (make-branch 2 4)
                                                         (make-branch 4 2)))))
; Test: Unbalanced
(balanced-mobile? (make-mobile (make-branch 3 4)
                               (make-branch 2
                                            (make-mobile (make-branch 6 2)
                                                         (make-branch 4 3)))))

; 4. Altercative representation
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; Only 2 selectors to change!
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; Test: Balanced
(balanced-mobile? (make-mobile (make-branch 3 4)
                               (make-branch 2
                                            (make-mobile (make-branch 2 4)
                                                         (make-branch 4 2)))))
; Test: Unbalanced
(balanced-mobile? (make-mobile (make-branch 3 4)
                               (make-branch 2
                                            (make-mobile (make-branch 6 2)
                                                         (make-branch 4 3)))))
