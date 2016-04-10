(define f
  (let ((called false))
    (lambda (x)
      (if called
        0
        (begin (set! called true)
               x)))))

(+ (f 0) (f 1))
; 1 -> right to left
