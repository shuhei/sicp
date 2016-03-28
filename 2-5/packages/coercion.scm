(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n)
                                 0))
  (define (scheme-number->scheme-number n) n)
  (define (complex->comples z) z)

  (put-coercion 'scheme-number 'complex
                scheme-number->complex)
  ; (put-coercion 'scheme-number 'scheme-number
                ; scheme-number->scheme-number)
  ; (put-coercion 'complex 'complex
                ; complex->comples)
)
