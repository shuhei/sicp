; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x) x)))

; Unlike the special form `if`, `new-if`'s arguments are all evaluated
; before `new-if` is applied. Thus the third argument of `sqrt-iter`
; introduces an infinute loop.
