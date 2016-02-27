(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

; Variable
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

; Sum
; Exercie 2.57
(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (apply make-sum (cddr s)))

; TODO: Simplify (+ (+ x y) z) to (+ x y z)
(define (make-sum . as)
  (let ((num (apply + (filter number? as)))
        (vars (remove number? as)))
    (let ((joined (if (= num 0)
                   vars
                   (cons num vars))))
         (cond ((null? joined) 0)
               ((null? (cdr joined)) (car joined))
               (else (cons '+ joined))))))

; Product
; Exercie 2.57
(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (apply make-product (cddr p)))

; TODO: Simplify (* (* x y) z) to (* x y z)
(define (make-product . ms)
  (let ((num (apply * (filter number? ms)))
        (vars (remove number? ms)))
    (let ((joined (cond ((= num 0) (list 0))
                        ((= num 1) vars)
                        (else (cons num vars)))))
      (cond ((null? joined) 1)
            ((null? (cdr joined)) (car joined))
            (else (cons '* joined))))))

; Exponentiation
; Exercie 2.56
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))

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
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(** x 4) 'x)
