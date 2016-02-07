; From Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (square x)
  (* x x))

(define (segment-length s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (sqrt (+ (square (- (x-point p1) (x-point p2)))
             (square (- (y-point p1) (y-point p2)))))))

; Rectangle 1
(define (make-rectangle p theta short long)
  (cons (cons p theta) (cons short long)))

(define (short-side rect)
  (car (cdr rect)))

(define (long-side rect)
  (cdr (cdr rect)))

(define rect
  (make-rectangle (make-point 3 4) 0 3 5))

; Rectangle 2
; (define (make-rectangle seg1 seg2)
  ; (let ((l1 (segment-length seg1))
        ; (l2 (segment-length seg2)))
    ; (if (< l1 l2)
      ; (cons seg1 seg2)
      ; (cons seg2 seg1))))

; (define (short-side rect)
  ; (segment-length (car rect)))

; (define (long-side rect)
  ; (segment-length (cdr rect)))

; (define rect
  ; (make-rectangle (make-segment (make-point 0 0) (make-point 3 0))
                  ; (make-segment (make-point 0 0) (make-point 0 5))))

; Perimeter and area
(define (perimeter rect)
  (* 2 (+ (short-side rect) (long-side rect))))

(define (area rect)
  (* (short-side rect) (long-side rect)))

; Test
(short-side rect)
(long-side rect)
(perimeter rect)
(area rect)
