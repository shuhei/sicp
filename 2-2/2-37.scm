(define (accumulate-n op init segs)
  (if (null? (car segs))
    ()
    (cons (fold-right op init (map car segs))
          (accumulate-n op init (map cdr segs)))))

(define (dot-product v w)
  (fold-right + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; Test
(dot-product (list 1 2 3) (list 3 4 5))
; 26

(matrix-*-vector (list (list 1 2 3)
                       (list 2 3 4))
                 (list 1 2 3))
; (14 20)

(transpose (list (list 1 2 3)
                 (list 2 3 4)))
; ((1 2) (2 3) (3 4))

(matrix-*-matrix (list (list 1 2 3)
                       (list 2 3 4))
                 (list (list 1 2)
                       (list 2 3)
                       (list 3 4)))
; ((14 20) (20 29))
