; Utilities
(define (enumerate-interval from to)
  (if (> from to)
    ()
    (cons from
          (enumerate-interval (+ 1 from) to))))

(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

; Prime test
(define (divides? m n)
  (= (remainder n m) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else
          (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; unique-pairs
(define (unique-pairs n)
   (flatmap
     (lambda (i)
       (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

; prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; Test
(prime-sum-pairs 6)
