(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset)
             (lambda (r) (set! x r)))
            (else
              (error "Unknown request: RAND" m))))))
