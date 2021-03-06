
;; 2.3.4 - Huffman Encoding Trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (raise (list "bad bit -- CHOOSE-BRANCH" bit)))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-in-set? symbol set)
  (cond ((null? set) #f)
        ((eq? symbol (car set)) #t)
        (else (symbol-in-set? symbol (cdr set)))))

(define (symbol-in-branch? symbol tree)
  (symbol-in-set? symbol (symbols tree)))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((symbol-in-branch? symbol (left-branch tree))
         (append (list 0)
                 (encode-symbol symbol (left-branch tree))))
        ((symbol-in-branch? symbol (right-branch tree))
         (append (list 1)
                 (encode-symbol symbol (right-branch tree))))
        (else (raise (list "Symbol not in tree -- ENCODE-SYMBOL" symbol)))))

;; Ex 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge trees)
  (if (= (length trees) 1)
      (car trees)
      (successive-merge
       (adjoin-set (make-code-tree (car trees) (cadr trees))
                   (cddr trees)))))
