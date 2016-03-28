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
(load-lib "packages/coercion")

; Arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (exp x y) (apply-generic 'exp x y))

(define (numer r) (apply-generic 'numer r))
(define (denom r) (apply-generic 'denom r))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? z) (apply-generic '=zero? z))

(define (raise z)
  (let ((tag (type-tag z)))
    ((get-raise tag) z)))

; Install
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-coercion-package)
(install-tower-coercion-package)

(begin
  ; Exercise 2.77
  (assert (= (magnitude (make-complex-from-real-imag 3 4))
             5))
  ; (magnitude '(complex rectangular 3 . 4))
  ; (apply-generic 'magnitude '(complex rectangular 3 . 4))
  ; (magnitude '(rectangular 3 . 4))
  ; (apply-generic 'magnitude '(rectangular 3 . 4))
  ; (magnitude(-rectangular) '(3 . 4))
  ; 5

  ; magnitude strips the complex tag and invoke magnitude again.
  ; Then it's applied to rectangular's magnitude.
  ; apply-generic is invoked twice.

  ; Exercise 2.78
  (assert (equal? (add 3 4) 7))
  (assert (equ? (add (make-rational 1 2)
                     (make-rational 1 4))
                (make-rational 3 4)))

  ; Exercise 2.79
  (assert (equ? (make-scheme-number 3)
                (make-scheme-number 3)))
  (assert (not (equ? (make-scheme-number 3)
                     (make-scheme-number 5))))

  (assert (equ? (make-rational 2 3)
                (make-rational 2 3)))
  (assert (not (equ? (make-rational 2 3)
                     (make-rational 3 2))))

  (assert (equ? (make-complex-from-real-imag 3 4)
                (make-complex-from-real-imag 3 4)))
  (assert (not (equ? (make-complex-from-real-imag 3 4)
                     (make-complex-from-real-imag 4 4))))

  (assert (equ? (make-complex-from-mag-ang 3 1)
                (make-complex-from-mag-ang 3 1)))
  (assert (not (equ? (make-complex-from-mag-ang 3 1)
                     (make-complex-from-mag-ang 3 2))))

  ; Exercise 2.80
  (assert (=zero? (make-scheme-number 0)))
  (assert (not (=zero? (make-scheme-number 3))))

  (assert (=zero? (make-rational 0 3)))
  (assert (not (=zero? (make-rational 2 3))))

  (assert (=zero? (make-complex-from-real-imag 0 0)))
  (assert (not (=zero? (make-complex-from-real-imag 0 1))))

  (assert (=zero? (make-complex-from-mag-ang 0 1)))
  (assert (not (=zero? (make-complex-from-mag-ang 1 1))))

  ; Exercise 2.81
  (assert (= (exp 2 3) 8))
  (assert (equal? (add (make-complex-from-real-imag 2 3)
                       (make-complex-from-real-imag 3 4))
                  (make-complex-from-real-imag 5 7)))

  ; Exercise 2.83
  (assert (equ? (raise 2)
                (make-rational 2 1)))
  (assert (equ? (raise (make-rational 1 2))
                (make-complex-from-real-imag 0.5 0)))
)

; Exercise 2.81
; 1.
(exp 2 3)
(exp (make-complex-from-real-imag 2 3)
     (make-complex-from-real-imag 3 4))
;No method for these types: APPLY-GENERIC (exp (complex complex))

; 2.
; Louis is not correct. It just works without modification to apply-generic.

; 3.
; No need to modify.

; Exercise 2.82
; # Coercing all arguments to the type of one of them.
; coercion table: rational->complex, scheme-number->complex, scheme-number->rational
; op table: foo: '(complex complex complex)
; (foo scheme-number scheme-number scheme-number)
; Then scheme-numbers are not going to be coerced to complex and the implementation is not used.

; Exercise 2.83
; See coercion.scm

(make-rational 1 2)
(raise (make-rational 1 2))
