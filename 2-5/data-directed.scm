(define table (make-empty-table))
(define coercion-table (make-empty-table))
(define raise-table (make-empty-table))

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

(define (get-coercion from to)
  (get-table (list from to)
             coercion-table))

(define (put-coercion from to item)
  (put-table (list from to)
             item
             coercion-table))

(define (get-raise from)
  (get-table from raise-table))

(define (put-raise from item)
  (put-table from item raise-table))

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
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (newline)
            (write-string "trying coercion: ")
            (write type1)
            (write-string " vs ")
            (write type2)
            (newline)
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else
                      (error "No method for these types: APPLY-GENERIC"
                             (list op type-tags))))))
          (else
              (error "No method for these types: APPLY-GENERIC"
                     (list op type-tags))))))))
