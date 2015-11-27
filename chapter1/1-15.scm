(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
        (if (not (> (abs angle) 0.1))
            angle
            (p (sine (/ angle 3.0)))))

; 1. 5 times
;
; 12.15 -> 1
; 4.05 -> 2
; 1.35 -> 3
; 0.45 -> 4
; 0.15 -> 5
; 0.05 -> fin
;
; or ceil(log(12.15 / 0.1, 3))

; 2. Θ(log n) space and Θ(log n) steps
