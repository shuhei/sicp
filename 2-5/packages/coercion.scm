(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n)
                                 0))
  ; Exercise 2.81.
  ; (define (scheme-number->scheme-number n) n)
  ; (define (complex->comples z) z)

  (put-coercion 'scheme-number 'complex
                scheme-number->complex)
  ; Exercise 2.81.
  ; (put-coercion 'scheme-number 'scheme-number
                ; scheme-number->scheme-number)
  ; (put-coercion 'complex 'complex
                ; complex->comples)
)

; Use scheme-number instead of integer and real.
(define (install-tower-coercion-package)
  (define (scheme-number->rational x)
    (make-rational x 1))
  ; TODO: r doesn't have a tag...
  (define (rational->complex r)
    (make-complex-from-real-imag (/ (numer r) (denom r))
                                 0))

  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'rational 'complex rational->complex)

  (put-raise 'scheme-number scheme-number->rational)
  (put-raise 'rational rational->complex)
)
