; Exercise 2.42

; Utility
(define (flatmap proc seq)
  (fold-right append () (map proc seq)))

(define (enumerate-interval from to)
  (if (> from to)
    ()
    (cons from
          (enumerate-interval (+ 1 from) to))))

(define (any? proc seq)
  (fold-right (lambda (x acc)
                (if acc acc (proc x)))
              false
              seq))

(any? even? (list 1 3 5 8))
(any? even? (list 1 3 5))
(any? even? (list 1 2 4 5))

; Components
(define empty-board
  ())

(define (safe? k positions)
  (let ((new (car positions))
        (rest (cdr positions)))
    (not (any? (lambda (position)
                 (or (= (car new) (car position))
                     (= (cdr new) (cdr position))
                     (= (- (car new) (cdr new))
                        (- (car position) (cdr position)))
                     (= (+ (car new) (cdr new))
                        (+ (car position) (cdr position)))))
               rest))))

(define (adjoin-position row k rest-of-queens)
  (cons (cons k row) rest-of-queens))

; Solve the problem!
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 1)
(queens 2)
(queens 3)
(queens 4)
(queens 5)
; (queens 8)

(length (queens 8))
; 92
(length (queens 9))
; 352
(length (queens 10))
; 724
(length (queens 11))
; Aborting!: maximum recursion depth exceeded

; Exercise 2.43
(define (louis-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map
              (lambda (rest-of-queens)
                      (adjoin-position new-row
                                       k
                                       rest-of-queens))
              (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(louis-queens 1)
(louis-queens 2)
(louis-queens 3)
(louis-queens 4)
(louis-queens 5)
(louis-queens 6)
; Slow
; (louis-queens 7)

; Because the Louis' version computes `(queen-cols (-k 1))` `board-size` times instead of once.
