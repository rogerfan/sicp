
;; 2.2.1 - Representing sequences
(cons 1 (cons 2 (cons 3 (cons 4 nil))))
(list 1 2 3 4)

(define test (list 1 2 3 4))
(car test)
(cdr test)
(car (cdr test))

(define (list-ref items n)
  (if (= n 0) (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items) 0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

;; Ex 2.17
(define (last-pair items)
  (if (null? (cdr items)) items
      (last-pair (cdr items))))

;; Ex 2.18
(define (reverse items)
  (if (null? items) items
      (cons (reverse (cdr items)) (car items))))

;; Ex 2.20
(define (same-parity a . items)
  (define (sp-builder items even)
    (cond ((null? items) items)
          ((equal? even (even? (car items)))
           (cons (car items) (sp-builder (cdr items) even)))
          ((not (equal? even (even? (car items))))
           (sp-builder (cdr items) even))))
  (sp-builder (cons a items) (even? a)))

;; Mapping
(define (map proc items)
  (if (null? items) nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; Ex 2.23
(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

;; 2.2.2 - Hierarchical Structures
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)

(length (list x x))
(count-leaves (list x x))

;; Ex 2.27
(define (reverse items)
  (if (null? items) items
      (cons (reverse (cdr items)) (car items))))

(define (deep-reverse tree)
  (if (pair? tree)
      (reverse (map deep-reverse tree))
      tree))

;; Ex 2.28
(define (fringe tree)
  (if (null? tree) nil
      (let ((first (car tree)))
        (if (not (pair? first))
            (cons first (fringe (cdr tree)))
            (append (fringe first) (fringe (cdr tree)))))))

;; Ex 2.30
(define (square-tree tree)
  (map
    (lambda (stree)
      (if (pair? stree) (square-tree stree)
          (* stree stree)))
    tree))

;; Ex 2.31
(define (tree-map proc tree)
  (map
    (lambda (stree)
      (if (pair? stree) (tree-map proc stree)
          (proc stree)))
    tree))
