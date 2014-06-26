
;; 1.2.1 - Factorial

; Recursive process
(define (factorial n)
  (cond ((= n 1) 1)
        (else (* n (factorial (- n 1))))))

; Iterative process
(define (factorial n)
  (define (fact-iter prod counter max)
    (if (> counter max)
        prod
        (fact-iter (* counter prod)
                   (+ counter 1)
                   max)))
  (fact-iter 1 1 n))


;; 1.2.1 - Fibonacci

; Recursive, evolves exponentially
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; Iterative, evolves linearly
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; Example - Number of ways to make change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Ex 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f n)
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (f-iter 2 1 0 n))

;; Ex 1.12 - Pascal's triangle
(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) col)
         (pascal (- row 1) (- col 1)))))

;; Ex 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 1.2.4 - Exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (define (expt-iter b counter prod)
    (if (= counter 0)
        prod
        (expt-iter b
                   (- counter 1)
                   (* b prod))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; Ex 1.16
(define (fast-expt b n)
  (define (fe-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fe-iter (square b) (/ n 2) a))
          (else (fe-iter b (- n 1) (* a b)))))
  (fe-iter b n 1))

;; Ex 1.17
(define (times a b)
  (cond ((= b 0) 0)
        (else (+ a (times a (- b 1))))))

(define (fast-times a b)
  (define (double x) (* x 2))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (fast-times (double a) (halve b)))
        (else (+ a (fast-times a (- b 1))))))

;; Ex 1.18
(define (fast-times a b)
  (define (ft-iter a b x)
    (define (double x) (* x 2))
    (define (halve x) (/ x 2))
    (cond ((= b 0) x)
          ((even? b) (ft-iter (double a) (halve b) x))
          (else (ft-iter a (- b 1) (+ x a)))))
  (ft-iter a b 0))

;; Ex 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (define (square x) (* x x))
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))  ; compute p'
                     (+ (* 2 p q) (square q))  ; compute q'
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
