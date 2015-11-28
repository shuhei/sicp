(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order: 18 times
;
; Let's say A(n) and B(n) as numbers of remainder calls in `a`/`b` at nth call of `gcd`.
; A(0) = 0
; B(0) = 0
; A(n + 1) = B(n)
; B(n + 1) = 1 + A(n) + B(n) = 1 + B(n - 1) + B(n)
;
; T(n), the total number of remainder calls in n gcd calls, is:
; T(n) = Σ(n)(B(k)) + A(n)
;
; B(n) = Fib(n) + 2(n - 2)
; A(n) = Fib(n - 1)  + 2(n - 3)
(gcd 206 40)
(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))) ; 1
(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(gcd (remainder 40 (remainder 206 40)) ; 3 (+2)
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; 7 (+4)
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
       0)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
    (...))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; 14 (+7)
2 ; 18 (+4)

; Applicative-order: 4 times
(gcd 206 40)
(gcd 40 6) ; 1
(gcd 6 4) ; 2
(gcd 4 2) ; 3
(gcd 2 0) ; 4
2
