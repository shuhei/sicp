(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter i acc)
    (if (= i 1)
      acc
      (iter (- i 1) (compose f acc))))
  (iter n f))

(define (average x y)
  (/ (+ x y) 2.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root n x)
  (define (f y)
    (/ x (expt y (- n 1))))
  (let ((damp-count (floor (log2 n))))
    (fixed-point ((repeated average-damp damp-count) f)
                 1.0)))

(nth-root 2 2.0)
; 1 damp
;Value: 1.4142135623746899

(nth-root 3 2.0)
; 1 damp
;Value: 1.259923236422975

(nth-root 4 2.0)
; 2 damps
;Value: 1.189207115002721

(nth-root 5 2.0)
; 2 damps
;Value: 1.1486967244204176

(nth-root 6 2.0)
; 2 damps
;Value: 1.1224648393618204

(nth-root 7 2.0)
; 2 damps
;Value: 1.1040857488809648

(nth-root 8 2.0)
; 3 damps
;Value: 1.090507732665258

(nth-root 16 2.0)
;Value: 1.0442737824274142

(nth-root 32 2.0)
;Value: 1.0218971486541175
