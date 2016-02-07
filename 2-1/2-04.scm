(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

; Test
(car (cons 3 5))
; (car (lambda (m) (m 3 5)))
; ((lambda (m) (m 3 5)) (lambda (p q) p))
; ((lambda (p q) p) 3 5)
; 3

(cdr (cons 3 5))
; (cdr (lambda (m) (m 3 5)))
; ((lambda (m) (m 3 5)) (lambda (p q) q))
; ((lambda (p q) q) 3 5)
; 5
