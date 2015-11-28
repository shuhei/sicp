(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Normal-order: 18 times
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
