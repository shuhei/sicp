; Church numerals
(define zero
  (lambda (f) (lambda (x) x)))

; We need the inverse of f to define subtract-1.
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; One and two
(define one
  (lambda (f) (lambda (x) (f x))))
; (add-1 zero)
; (lambda (f) (lambda (x) (f (((lambda (ff) (lambda (xx) xx)) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (xx) xx) x))))
; (lambda (f) (lambda (x) (f x)))

(define two
  (lambda (f) (lambda (x) (f (f x)))))
; (add-1 one)
; (lambda (f) (lambda (x) (f (((lambda (ff) (lambda (xx) (ff xx))) f) x))))
; (lambda (f) (lambda (x) (f (((lambda (xx) (f xx))) x))))
; (lambda (f) (lambda (x) (f (f x))))

; +
(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; Convert to ordinary number
(define (to-num n)
  ((n (lambda (x) (+ x 1))) 0))

; Test
(to-num zero)
(to-num one)
(to-num two)
(to-num (plus (plus one two) two))
