
(define (equal? l1 l2)
  (cond ((symbol? l1) (eq? l1 l2))
        ((list? l1) (and (equal? (car l1) (car l2))
                         (equal? (cdr l1) (cdr l2))))))


;; Sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; Ex 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; Ex 2.59 - Alternate
(define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))
(define (union-set set1 set2)
  (accumulate cons
              set2
              (filter (lambda (x) (not (element-of-set? x set2)))
                      set1)))


;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

; Ex 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set))
         (cons (car set)
               (adjoin-set x (cdr set))))
        ((< x (car set))
         (cons x set))))

; Ex 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))))))


;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch tree)))
        ((> x (entry set)) (element-of-set? x (right-branch tree)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (list->tree elements)
  (define (quotient a b) (floor (/ a b)))
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts))))))))
  (car (partial-tree elements (length elements))))

; Ex 2.65
(define (union-set set1 set2)
  (define (union-set-list s1 s2)
    (cond ((null? s1) s2)
          ((null? s2) s1)
          (else
           (let ((x1 (car s1)) (x2 (car s2)))
             (cond ((= x1 x2) (cons x1 (union-set-list (cdr s1) (cdr s2))))
                   ((< x1 x2) (cons x1 (union-set-list (cdr s1) s2)))
                   ((> x1 x2) (cons x2 (union-set-list s1 (cdr s2)))))))))
  (list->tree (union-set-list (tree->list set1)
                              (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersection-set-list s1 s2)
    (if (or (null? s1) (null? s2))
        '()
        (let ((x1 (car s1)) (x2 (car s2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set-list (cdr s1) (cdr s2))))
                ((< x1 x2)
                 (intersection-set-list (cdr s1) s2))
                ((> x1 x2)
                 (intersection-set-list s1 (cdr s2)))))))
  (list->tree (intersection-set-list
               (tree->list set1)
               (tree->list set2))))

(define (lookup given-key records)
  (cond ((null? records) #f)
        ((= given-key (key (entry records))) (entry records))
        ((< given-key (key (entry records)))
         (lookup given-key (left-branch records)))
        ((> given-key (key (entry records)))
         (lookup given-key (right-branch records)))))
