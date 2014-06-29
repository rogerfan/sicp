
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
