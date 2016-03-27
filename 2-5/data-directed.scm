(define table (make-empty-table))

(define (get op type)
  (let ((op-table (get-table op table)))
    (if op-table
      (get-table type op-table)
      false)))

(define (put op type item)
  (let ((op-table (get-table op table)))
    (if op-table
      (put-table type item op-table)
      (put-table op
                 (make-table (list (cons type item)))
                 table))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum: CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))
