
;; 2.3.1 - Quotation
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; Ex 2.54
(define (equal? l1 l2)
  (cond ((symbol? l1) (eq? l1 l2))
        ((list? l1) (and (equal? (car l1) (car l2))
                         (equal? (cdr l1) (cdr l2))))))

;; 2.3.2 - Symbolic Differentiation
