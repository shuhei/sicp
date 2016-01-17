(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (iter-expt b n)
  ; `a * (b ^ n)` is the invariant quantity.
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n)) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1))))
  (expt-iter 1 b n))

; Some tests
(define (test b n expected)
  (define actual (fast-expt b n))
  (define expectation
    (string-append (number->string b)
                   "^"
                   (number->string n)
                   " = "
                   (number->string expected)))
  (if (= actual expected)
      (string-append "OK: " expectation)
      (string-append "NG: expected "
                     expectation
                     " but got "
                     (number->string actual))))

(test 5 0 1)
(test 9 2 82)
(test 3 3 27)
(test 2 10 1024)
(test 5 8 390625)
