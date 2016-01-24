(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (if (< d 0)
    (make-rat (* n -1) (* d -1))
    (let ((g (gcd n d)))
      (cons (/ n g)
            (/ d g)))))

(print-rat (make-rat 2 3))
; 2/3
(print-rat (make-rat 2 -3))
; -2/3
(print-rat (make-rat -2 3))
; -2/3
(print-rat (make-rat -2 -3))
; 2/3
