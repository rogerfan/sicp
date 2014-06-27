
;; 1.3.1
(define (sum term next a b)
  (cond ((> a b) 0)
        (else (+ (term a)
                 (sum term next (next a) b)))))

(define (cube x) (* x x x))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube inc a b))

(define (identity x) x)
(define (sum-int a b)
  (sum identity inc a b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2)) b)
     dx))

;; Ex 1.29 - Simpson's Rule for approximating integration
(define (integral-simpson f a b n)
  (define (yk k h)
    (f (+ a (* k h))))
  (define (coef k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (coef k)
       (yk k (/ (- b a) n))))
  (* (/ (/ (- b a) n) 3)
     (sum term inc 1 n)))

;; Ex 1.30
(define (itersum term next a b)
  (define (iter a result)
    (cond ((> a b) result)
          (else (iter (next a) (+ result (term a))))))
  (iter a 0))

;; Ex 1.31 - Product
(define (prod term next a b)
  (cond ((> a b) 1)
        (else (* (term a)
                 (prod term next (next a) b)))))

(define (iterprod term next a b)
  (define (iter a result)
    (cond ((> a b) 1)
          (else (iter (next a) (* result (term a))))))
  (iter a 1))

(define (factorial n)
  (prod identity inc 1 n))


;; Ex 1.32 - Accumulator
(define (accum combiner null-val term next a b)
  (cond ((> a b) null-val)
        (else (combiner (term a)
                        (accum combiner null-val term next (next a) b)))))

(define (iteraccum combiner null-val term next a b)
  (define (iter a result)
    (cond ((> a b) result)
          (else (iter (next a)
                      (combiner result (term a))))))
  (iter a null-val))

;; Ex 1.33
(define (filter-accum comb null-val filter term next a b)
  (if (> a b) null-val
      (comb (filter-accum comb null-val filter term next (next a) b)
            (if (filter a) (term a)
                null-val))))

(define (smallest-div n)
  (define (square x) (* x x))
  (define (remainder a b)
    (- a (* b (floor (/ a b)))))
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (find-div n test)
    (cond ((> (square test) n) n)
           ((divides? test n) test)
           (else (find-div n (+ test 1)))))
  (find-div n 2))

(define (prime? n)
  (if (= n 1) #f
      (= n (smallest-div n))))

(define (ss-prime a b) (filter-accum + 0 prime? square inc a b))

;; 1.3.2 - Lambda and Let
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square q))
       (* y b)
       (* a b))))

;; 1.3.3 - Half-interval method
(define (search f neg pos)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.001))
  (let ((midpoint (/ (+ neg pos) 2)))
    (if (close-enough? neg pos) midpoint
        (let ((test-val (f midpoint)))
          (cond ((> test-val 0) (search f neg midpoint))
                ((< test-val 0) (search f midpoint pos))
                (else midpoint))))))

(define (halfint-zero f a b)
  (let ((a-val (f a))
        (b-val (f b)))
    (cond ((and (< a-val 0) (> b-val 0))
           (search f a b))
          ((and (> a-val 0) (< b-val 0))
           (search f b a))
          ((= a-val 0) a)
          ((= b-val 0) b)
          (else error "Values are not of opposite sign" a b))))

(define (testf x)
  (- (* x x) 3))

;; 1.3.4
(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-trans g)
  (lambda (x)
    (- x (/ (g x) ((deriv g 0.000001) x)))))
(define (newton-method g guess)
  (fixed-point (newton-trans g) guess))

;; Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Ex 1.43
(define (repeated f n)
  (if (= n 0) (lambda (x) x)
      (compose f (repeated f (- n 1)))))

;; Ex 1.44
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (nsmooth f n dx)
  ((repeated (lambda (g) (smooth g dx)) n) f)

;; Ex 1.46
(define (iter-improve good-enough? improve)
  (lambda (x)
    (let ((xim (improve x)))
      (if (close-enough? x xim) xim
          ((iter-improve close-enough? improve) xim)))))
