; Infra
(define (put op type item)
  ())

(define (get op type)
  ())

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

; deriv
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 1. numbers and variables are not pairs and cannot have type-tags.

; 2.
(define (install-sum-package)
  (define (deriv-sum exp)
    (make-sum
      (deriv (addend exp) var)
      (deriv (augend exp) var)))
  (put 'deriv '+ deriv-sum))

(define (install-product-package)
  (define (deriv-product exp)
    (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (deriv (multiplier exp) var)
        (multiplicand exp))))
  (put 'deriv '* deriv-sum))

; 3.
(define (install-exponentiation-package)
  (define (deriv-exponentiation exp)
    (make-product
      (make-product
        (exponent exp)
        (make-exponentiation
          (base exp)
          (- (exponent exp) 1)))
      (deriv (base exp))))
  (put 'deriv '^ deriv-exponentiation))

; 4.
((get (operator exp) 'deriv)
 (operands exp)
 var)

; Change general operation.
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get (operator exp) 'deriv)
                (operands exp)
                var))))

; Change each `put` call in package installers.
(put '+ deriv deriv-sum)
