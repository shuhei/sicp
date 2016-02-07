; Exercise 2.7
(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

; From the body text
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

; Exercise 2.9
; Sum:
; (a ± wa) + (b ± wb) = (a + b) ± (wa + wb)
; wab = wa + wb
;
; Multiplication:
; If a > 0 and b > 0,
; (a ± wa) * (b ± wb) = (a * b) ± (a * wb + b * wa + wa * wb)

; Exercise 2.10
(define (span-zero? interval)
  (and (<= (lower-bound interval) 0)
       (<= 0 (upper-bound internval))))

(define (div-interval x y)
  (if (span-zero? y)
    (error "Interval spanning zero:"
           (lower-bound y)
           "to"
           (upper-bound y))
    (mul-interval x y)))

; Exercise 2.11
(define (plus? x)
  (>= x 0))

(define (minus? x)
  (< x 0))

; TODO: Simpler way?
(define (mul-interval-9 x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (minus? xl) (minus? xu))
           (cond ((and (minus? yl) (minus? yu))
                  (make-interval (* xu yu) (xl yl)))
                 ((and (minus? yl) (plus? yu))
                  (make-interval (* xl yu) (xl yl)))
                 ((and (minus? yl) (plus? yu))
                  (make-interval (* xl yu) (xu yl)))))
          ((and (minus? xl) (plus? xu))
           (cond ((and (minus? yl) (minus? yu))
                  (make-interval (* xu yl) (xl yl)))
                 ((and (minus? yl) (plus? yu))
                  (mul-interval x y))
                 ((and (minus? yl) (plus? yu))
                  (make-interval (* xl yu) (xu yu)))))
          ((and (plus? xl) (plus? xu))
           (cond ((and (minus? yl) (minus? yu))
                  (make-interval (* xu yl) (xl yu)))
                 ((and (minus? yl) (plus? yu))
                  (make-interval (* xu yl) (xu yu)))
                 ((and (minus? yl) (plus? yu))
                  (make-interval (* xl yl) (xu yu))))))))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c p)
  (make-center-width c
                     (/ (* c p) 100)))

(define (percent i)
  (* (/ (width i)
        (center i))
     100))

; Exercise 2.13
; (a ± wa) * (b ± wb) = (a * b) ± (a * wb + b * wa + wa * wb)
; ≒ (a * b) ± (a * wb + b * wa)
(define (mul-interval-simple x y)
  (let ((xc (center x))
        (xw (width x))
        (yc (center y))
        (yw (width y)))
    (make-center-width (* xc yc)
                       (+ (* xc yw) (* yc xw)))))
