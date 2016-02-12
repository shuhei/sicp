#lang racket/gui

; Exercise 2.47
; List
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
; or (first frame)

(define (edge1-frame frame)
  (cadr frame))
; or (second frame)

(define (edge2-frame frame)
  (caddr frame))
; or (third frame)

; Nested pair
(define (make-frame-nested origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-nested frame)
  (car frame))

(define (edge1-frame-nested frame)
  (cadr frame))

(define (edge2-frame-nested frame)
  (cddr frame))


; Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


; Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;Exercise 2.45
; (define right-split (split beside below))
; (define up-split (split below beside))

(define (split op1 op2)
  (define (sp painter n)
    (if (= n 0)
      painter
      (let ((smaller (sp painter (- n 1))))
        (op1 painter
             (op2 smaller smaller)))))
  sp)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

; Graphics
; https://ericscrivner.me/2015/05/the-sicp-picture-language-in-racket/
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (let ((start-coord-map ((frame-coord-map frame) (start-segment segment)))
              (end-coord-map ((frame-coord-map frame) (end-segment segment))))
          (line (make-posn (xcor-vect start-coord-map) (ycor-vect start-coord-map))
                (make-posn (xcor-vect end-coord-map) (ycor-vect end-coord-map)))))
      segment-list)))

(define unit-frame (make-frame (make-vect 0 500)
                               (make-vect 500 0)
                               (make-vect 0 -500)))


(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter
              (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter
             (beside smaller smaller)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)
                       (tr painter)))
          (bottom (beside (bl painter)
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity
                                  flip-vert
                                  identity
                                  flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert)))
    (combine4 (corner-split painter n))))

; Exercise 2.49
; 1.
(define (outline f)
  (let ((origin (origin-frame f))
        (edge1 (edge1-frame f))
        (edge2 (edge2-frame f)))
    (let ((e1 (add-vect origin edge1))
          (e2 (add-vect origin edge2))
          (opposite (add-vect (add-vect origin edge1) edge2)))
      (segments->painter
        (list (make-segment origin e1)
              (make-segment origin e2)
              (make-segment e1 opposite)
              (make-segment e2 opposite))))))

; 2.
(define (draw-x f)
  (let ((origin (origin-frame f))
        (edge1 (edge1-frame f))
        (edge2 (edge2-frame f)))
    (let ((e1 (add-vect origin edge1))
          (e2 (add-vect origin edge2))
          (opposite (add-vect (add-vect origin edge1) edge2)))
      (segments->painter
        (list (make-segment origin opposite)
              (make-segment e1 e2))))))
; 3.
(define (midpoint v1 v2)
  (scale-vect (/ 1 2)
              (add-vect v1 v2)))

(define (diamond f)
  (let ((origin (origin-frame f))
        (edge1 (edge1-frame f))
        (edge2 (edge2-frame f)))
    (let ((e1 (add-vect origin edge1))
          (e2 (add-vect origin edge2))
          (opposite (add-vect (add-vect origin edge1 edge2))))
      (let ((p1 (midpoint origin e1))
            (p2 (midpoint origin e2))
            (p3 (midpoint e2 opposite))
            (p4 (midpoint e1 opposite)))
        (segments->painter
          (list (make-segment p1 p2)
                (make-segment p1 p4)
                (make-segment p2 p3)
                (make-segment p3 p4)))))))
; 4.
(define wave
  (segments->painter
    (list (make-segment (make-vect 0.4 1.0) (make-vect 0.3 0.9))
          (make-segment (make-vect 0.3 0.9) (make-vect 0.4 0.8))
          (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
          (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.8))
          (make-segment (make-vect 0.4 0.8) (make-vect 0.2 0.7))
          (make-segment (make-vect 0.2 0.7) (make-vect 0.1 0.5))
          (make-segment (make-vect 0.1 0.5) (make-vect 0.0 0.6))
          (make-segment (make-vect 0.0 0.5) (make-vect 0.1 0.4))
          (make-segment (make-vect 0.1 0.4) (make-vect 0.3 0.6))
          (make-segment (make-vect 0.3 0.6) (make-vect 0.3 0.3))
          (make-segment (make-vect 0.3 0.3) (make-vect 0.2 0.0))
          (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3))
          (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0.0))
          (make-segment (make-vect 0.8 0.0) (make-vect 0.7 0.3))
          (make-segment (make-vect 0.7 0.3) (make-vect 0.7 0.6))
          (make-segment (make-vect 0.7 0.6) (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.6) (make-vect 0.8 0.7))
          (make-segment (make-vect 0.8 0.7) (make-vect 0.6 0.8)))))

; Exercise 2.50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

; Exercise 2.51
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter
                        painter1
                        (make-vect 0.0 0.0)
                        split-point
                        (make-vect 0.0 1.0)))
          (paint-right (transform-painter
                         painter2
                         split-point
                         (make-vect 1.0 0.0)
                         (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; Straightforward
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter
                        painter1
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        split-point))
          (paint-top (transform-painter
                       painter2
                       split-point
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; With beside and rotations
(define (below-with-beside painter1 painter2)
  (rotate90
    (lambda (frame)
      (beside (rotate270 frame)
              (rotate270 frame)))))

; Exercise 2.52
; 1.
(define wave-smile
  (segments->painter
    (list (make-segment (make-vect 0.4 1.0) (make-vect 0.3 0.9))
          (make-segment (make-vect 0.3 0.9) (make-vect 0.4 0.8))
          (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
          (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.8))
          (make-segment (make-vect 0.4 0.8) (make-vect 0.2 0.7))
          (make-segment (make-vect 0.2 0.7) (make-vect 0.1 0.5))
          (make-segment (make-vect 0.1 0.5) (make-vect 0.0 0.6))
          (make-segment (make-vect 0.0 0.5) (make-vect 0.1 0.4))
          (make-segment (make-vect 0.1 0.4) (make-vect 0.3 0.6))
          (make-segment (make-vect 0.3 0.6) (make-vect 0.3 0.3))
          (make-segment (make-vect 0.3 0.3) (make-vect 0.2 0.0))
          (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3))
          (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0.0))
          (make-segment (make-vect 0.8 0.0) (make-vect 0.7 0.3))
          (make-segment (make-vect 0.7 0.3) (make-vect 0.7 0.6))
          (make-segment (make-vect 0.7 0.6) (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.6) (make-vect 0.8 0.7))
          (make-segment (make-vect 0.8 0.7) (make-vect 0.6 0.8))
          (make-segment (make-vect 0.45 0.85) (make-vect 0.5 0.82))
          (make-segment (make-vect 0.5 0.82) (make-vect 0.55 0.85)))))
; 2.
(define (corner-split-simple painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left up)
            (bottom-right right)
            (corner (corner-split-simple painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

; 3.
(define (corner-split-outer painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split (rotate270 painter) (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split-outer painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit-outer painter n)
  (let ((combine4 (square-of-four flip-horiz
                                  identity
                                  rotate180
                                  flip-vert)))
    (combine4 (corner-split-outer painter n))))

(define wave2
  (beside wave (flip-vert wave)))

(define wave4
  (below wave2 wave2))

(define rect
  (outline (make-frame (make-vect 0.25 0.25)
                       (make-vect 0.5 0.0)
                       (make-vect 0.0 0.5))))

(define x
  (draw-x (make-frame (make-vect 0.0 0.0)
                      (make-vect 1.0 0.0)
                      (make-vect 0.0 1.0))))


; ((square-limit wave 3) unit-frame)
((square-limit-outer wave-smile 3) unit-frame)
; ((corner-split wave4 2) unit-frame)
; ((corner-split-simple x 4) unit-frame)
; (wave-smile unit-frame)
