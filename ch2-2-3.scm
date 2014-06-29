
;; 2.2.3 - Sequences as Conventional Interfaces
(define (filter predicate items)
  (cond ((null? items) nil)
        ((predicate (car items))
         (cons (car items)
               (filter predicate (cdr items))))
        (else (filter predicate (cdr items)))))

(define (accum proc init items)
  (if (null? items) init
      (proc (car items)
            (accum proc init (cdr items)))))

(define (enum-interval low high)
  (if (> low high) nil
      (cons low (enum-interval (+ low 1) high))))

(define (enum-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enum-tree (car tree))
                      (enum-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accum +
         0
         (map (lambda (x) (* x x))
              (filter odd?
                      (enum-tree tree)))))
(sum-odd-squares (list 2 (list 1 (list 6 4)) (list 3 5)))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (even-fibs n)
  (accum cons
         nil
         (filter even?
                 (map fib
                      (enum-interval 0 n)))))

(define (list-fib-squares n)
  (accum cons
         nil
         (map (lambda (x) (* x x))
              (enum-interval 0 n))))

(define (prod-squares-odd items)
  (accum *
         1
         (map (lambda (x) (* x x))
              (filter odd?
                      items))))

;; Ex 2.33
(define (map-alt proc sequence)
  (accum (lambda (first rest)
           (cons (proc first)
                 rest))
         nil
         sequence))

(define (append-alt seq1 seq2)
  (accum cons seq2 seq1))

(define (length-alt sequence)
  (accum (lambda (new count) (+ count 1))
          0
          sequence))

;; Ex 2.35
(define (count-leaves tree)
  (define (enum-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enum-tree (car tree))
                        (enum-tree (cdr tree))))))
  (accum (lambda (new count) (+ count 1))
         0
         (enum-tree tree)))

(define (count-leaves tree)
  (accum +
         0
         (map (lambda (node)
                (if (pair? node)
                    (count-leaves node)
                    1))
              tree)))

;; Ex 2.36
(define testseqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; This one does it in the wrong dimension, accumulating each seq rather than
; the nth element of each seq
(define (accum-n op init seqs)
  (map (lambda (seq)
         (accum op init seq))
       seqs))

; This is how the problem is worded
(define (accum-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accum op init (map car seqs))
            (accum-n op init (map cdr seqs)))))

;; Ex 2.37
(define v1 (list 1 3 5))
(define v2 (list 3 6 3))
(define m1 (list (list 1 2 3) (list 2 3 4)))
(define m2 (list (list 1 2) (list 2 3) (list 3 4)))

(define (dot-prod v w)
  (accum + 0 (map * v w)))
(define (m*v m v)
  (map (lambda (row) (dot-prod row v)) m))
(define (transpose m)
  (accum-n cons nil m))
(define (m*m m1 m2)
  (let ((n-cols (transpose m2)))
    (map (lambda (row) (m*v n-cols row))
         m1)))

;; Nested Mappings
(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
