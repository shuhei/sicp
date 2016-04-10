(define (make-monitored f)
  (define count 0)
  (lambda (v)
    (cond ((eq? v 'how-many-calls?)
           count)
          ((eq? v 'reset-count)
           (set! count 0))
          (else
            (begin
              (set! count (+ count 1))
              (f v))))))

(begin
  (define s (make-monitored sqrt))
  (assert (= (s 'how-many-calls?) 0))
  (assert (= (s 100) 10))
  (assert (= (s 'how-many-calls?) 1))
  (assert (= (s 4) 2))
  (assert (= (s 'how-many-calls?) 2))
  (assert (= (s 'how-many-calls?) 2))
  (s 'reset-count)
  (assert (= (s 'how-many-calls?) 0)))
