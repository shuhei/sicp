; Ackermann's function
(define (A x y)
        (cond ((= y 0) 0)
              ((= x 0) (* 2 y))
              ((= y 1) 2)
              (else (A (- x 1)
                       (A x (- y 1))))))
(A 1 10)
; 1024
(A 2 4)
; 65536
(A 3 3)
; 65536


; Applicative-order application of (A 1 10)
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024


; -- f
(define (f n) (A 0 n))

(f n)
(A 0 n)
(cond ((= n 0) 0)
      (else (* 2 n)))
; f(n) = 2n

(define (g n) (A 1 n))


; -- g
(g n)
(A 1 n)
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 0 (A 1 (- n 1)))))
; g(0) = 0
; g(1) = 2
; g(n) = 2 * g(n - 1) = 2 ^ n


; -- h
(define (h n) (A 2 n))

(h n)
(A 2 n)
(cond ((= n 0) 0)
      ((= n 1) 2)
      (else (A 1 (A 2 (- n 1)))))

; h(0) = 0
; h(1) = 2
; h(n) = 2 ^ h(n - 1) = 2 ↑↑ n (Knuth's up-arrow notation)
