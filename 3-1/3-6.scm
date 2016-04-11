(define random-init 41432)
(define (random-update x)
  (modulo (+ (* 16598013 x) 12820163)
          16777216))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (random-update x))
                    x))
            ((eq? m 'reset)
             (lambda (r) (begin
                           (set! x r)
                           ())))
            (else
              (error "Unknown request: RAND" m))))))

(begin
  ((rand 'reset) 123)
  (define r1 (rand 'generate))
  (define r2 (rand 'generate))
  (assert (not (= r1 r2)))
  ((rand 'reset) 123)
  (assert (= (rand 'generate) r1))
  (assert (= (rand 'generate) r2))
  ((rand 'reset) 123)
  (assert (= (rand 'generate) r1))
  (assert (= (rand 'generate) r2)))
