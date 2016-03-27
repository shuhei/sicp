; TODO: Any way to get the current file's path?
(define (load-lib name)
  (load (string-append (->namestring (pwd))
                       "2-5/"
                       name
                       ".scm")))
(load-lib "table")
(load-lib "data-directed")

(load-lib "packages/scheme-number")
(load-lib "packages/rational")
(load-lib "packages/rectangular")
(load-lib "packages/polar")
(load-lib "packages/complex")

; Arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; Install
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; Exercise 2.77
(magnitude (make-complex-from-real-imag 3 4))
; (magnitude '(complex rectangular 3 . 4))
; (apply-generic 'magnitude '(complex rectangular 3 . 4))
; (magnitude '(rectangular 3 . 4))
; (apply-generic 'magnitude '(rectangular 3 . 4))
; (magnitude(-rectangular) '(3 . 4))
; 25

; magnitude strips the complex tag and invoke magnitude again.
; Then it's applied to rectangular's magnitude.
; apply-generic is invoked twice.

; Exercise 2.78
(add 3 4)
