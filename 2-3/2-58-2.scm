(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (take-until separator xs)
  (define (iter xs)
    (cond ((null? xs)
           ())
          ((eq? separator (car xs))
           ())
          (else
            (cons (car xs) (iter (cdr xs))))))
  (iter xs))

(define (take-after separator xs)
  (define (iter xs)
    (cond ((null? xs)
           ())
          ((eq? separator (car xs))
           (cdr xs))
          (else
            (iter (cdr xs)))))
  (iter xs))

(define (split separator xs)
  (fold-right (lambda (x acc)
                (if (eq? x separator)
                  (cons () (cons (car acc) (cdr acc)))
                  (cons (cons x (car acc)) (cdr acc))))
              (cons () ())
              xs))
(split '+ '(1 + 2 + 3 + 4 * 6 + 8))
; ((1) (2) (3) (4 * 6) (8))

(define (join separator xs)
  (define (iter xs)
    (if (null? xs)
      ()
      (cons separator
            (cons (car xs)
                  (iter (cdr xs))))))
  (if (null? xs)
    ()
    (cons (car xs)
          (iter (cdr xs)))))
(join '+ '(x y z))
; (x + y + z)

(define (unwrap-if-single x)
  (if (and (list? x)
           (= (length x) 1))
    (car x)
    x))
(unwrap-if-single '(x y z))
; (x y z)
(unwrap-if-single '(x))
; x

(define (flat-map f xs)
  (fold-right append
              ()
              (map f xs)))

; Variable
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

; Sum
; If you find a `+` in a list, it's a sum.
(define (sum? x)
  (and (list? x)
       (not (not (member '+ x)))))
(sum? '(a + b))
; #t
(sum? '(a + b + c * d))
; #t
(sum? '(a * (b + c) * d))
; #f

(define (addend s)
  (unwrap-if-single (take-until '+ s)))

(define (augend s)
  (unwrap-if-single (take-after '+ s)))

(define (unwrap-sums xs)
  (if (list? xs)
    (flat-map (lambda (x)
                (if (sum? x)
                  (map unwrap-if-single (split '+ x))
                  (list x)))
              xs)
    xs))
(unwrap-sums '((a + b) c (d + e)))
; (a b c d e)
(unwrap-sums '((x + y * z ) a))
; (x (y * z) a)

(define (make-sum . as)
  (let ((unwrapped (unwrap-sums as)))
    (let ((num (apply + (filter number? unwrapped)))
          (vars (remove number? unwrapped)))
      (let ((joined (if (= num 0)
                      vars
                      (cons num vars))))
        (cond ((null? joined)
               0)
              ((null? (cdr joined))
               (car joined))
              (else
                (flat-map (lambda (x)
                            (if (product? x)
                              x
                              (list x)))
                          (join '+ joined))))))))

; Product
; If you find a `*` and no other weaker operator, `+` in this case, it's a product.
; (a + b + c * d) -> not a product
; (a * b + c) -> not a product
; (a * b * c) -> product
; (a * (b + c + d) * e) -> product
(define (product? x)
  (and (list? x)
       (member '* x)
       (not (member '+ x))))

(define (multiplier p)
  (unwrap-if-single (take-until '* p)))

(define (multiplicand p)
  (unwrap-if-single (take-after '* p)))

(define (unwrap-products xs)
  (if (list? xs)
    (flat-map (lambda (x)
              (if (product? x)
                (map unwrap-if-single (split '* x))
                (list x)))
            xs)
    xs))
(unwrap-products '((x * y) z))
; (x y z)
(unwrap-products '((a + b) c (d * e)))
; ((a + b) c d e)

(define (make-product . ps)
  (let ((unwrapped (unwrap-products ps)))
    (let ((num (apply * (filter number? unwrapped)))
          (vars (remove number? unwrapped)))
      (let ((joined (cond ((= num 0) (list 0))
                          ((= num 1) vars)
                          (else (cons num vars)))))
        (cond ((null? joined) 0)
              ((null? (cdr joined)) (car joined))
              (else (join '* joined)))))))

; Test
(make-sum '(a + b) '(c + d))
; (a + b + c + d)
(make-sum '(a * b) '(c + d))
; (a * b + c + d)
(make-sum '(a * (b + c)) '(d * e))
; (a * (b + c) + d * e)

(make-product '(a * b) '(c * d * e))
; (a * b * c * d * e)
(make-product '(a * (b + c)) '(d * e))
; (a * (b + c) * d * e)

; Derivative
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))

(deriv '(x + 3) 'x)
(deriv '((x + 3) + x + (x + 2)) 'x)
(deriv '((x + 1) * (x + 2)) 'x)
(deriv '((x + 1) * (x * x + 2)) 'x)
(deriv '(x * y) 'x)
(deriv '(x * y + x + 3) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '((x + 1) * (x + 2)) 'x)
(deriv '(x + (x + 1) * (x * x + 2 * x + 3) + 4) 'x)
