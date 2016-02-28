; Leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

; Tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; Decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      ()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits)
                    next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

; Set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    ()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
; adabbca
; TODO: Why lowercase?

; Encode
(define (encode message tree)
  (if (null? message)
    ()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; Exercise 2.68
(define (encode-symbol sym tree)
  (define (enc tr)
    (if (leaf? tr)
      ()
      (let ((left (left-branch tr))
            (right (right-branch tr)))
        (cond ((member sym (symbols left))
               (cons 0 (enc left)))
              ((member sym (symbols right))
               (cons 1 (enc right)))
              (else
                (error "symbol not found: ENCODE-SYMBOL" sym))))))
  (enc tree))

(define (flatten items)
  (fold-right append () items))

(define (encode-symbols syms tree)
  (flatten (map (lambda (sym) (encode-symbol sym tree))
                syms)))

(encode-symbols '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (cond ((null? leafs)
         ())
        ((null? (cdr leafs))
         (car leafs))
        (else
          (let ((l1 (car leafs))
                (l2 (cadr leafs)))
            (successive-merge (adjoin-set (make-code-tree l1 l2)
                                          (cddr leafs)))))))

; Test
(generate-huffman-tree '((A 4)))
(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

; Exercise 2.70
(define freqs
  '((A 2) (NA 16) (BOOM 1) (SHA  3) (GET  2) (YIP  9) (JOB  2) (WAH  1)))

(define lyrics
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip
    yip yip yip yip yip
    Sha boom))

(encode-symbols lyrics (generate-huffman-tree freqs))
(length (encode-symbols lyrics (generate-huffman-tree freqs)))
; 84 bits

; If fixed-length, a symbol is represented by 3 bits.
(* 3 (length lyrics))
; 108 bits
