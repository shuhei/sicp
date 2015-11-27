(define (count-change amount)
        (cc amount 5))

(define (cc amount kinds-of-coins)
        (cond ((= amount 0) 1)
              ((or (< amount 0) (= kinds-of-coins 0)) 0)
              (else (+ (cc amount (- kinds-of-coins 1))
                       (cc (- amount (first-denomination kinds-of-coins))
                           kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
              ((= kinds-of-coins 2) 5)
              ((= kinds-of-coins 3) 10)
              ((= kinds-of-coins 4) 25)
              ((= kinds-of-coins 5) 50)))

; space: O(n)
; steps: Θ(n ^ 2) ?

; -- Answer by Torii-san.
;
; steps, space:
; (cc n 1) ∈ Θ(n)
;
; steps:
; (cc n 2) = (cc n 1) + (cc (n - 5) 2) + 1
;          = (cc n 1) + (cc (n - 5) 1) + (cc (n - 10) 2) + 2
;          <= (cc n 1) + n/5 * (cc n 1) + n/5
;          ∈ O(n^2)
; (cc n 3) = (cc n 2) + ...
;          ∈ O(n^3)
;
; space:
; O(n)
