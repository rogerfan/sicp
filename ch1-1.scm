
; Ex 1.3
(define (sum2sq a b) (+ (* a a) (* b b)))
(define (bothlargerthan a b c) (and (> a c) (> b c)))
(define (ss2 a b c)
  (cond
    ((bothlargerthan a b c) (sum2sq a b))
    ((bothlargerthan a c b) (sum2sq a c))
    ((bothlargerthan b c a) (sum2sq b c))))


; 1.1.7 Example - Newton's method for square roots
(define (sqrt-iter guess x)
  (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x) (sqrt-iter 1.0 x))

(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))
(define (square x) (* x x))


; Ex 1.7 - Alternative good-enough
(define (sqrt-iter2 guess guess_prev x)
  (if (good-enough2 guess guess_prev)
      guess
      (sqrt-iter2 (improve guess x) guess x)))
(define (sqrt2 x) (sqrt-iter2 1.0 0.1 x))

(define (good-enough2 guess guess_prev)
  (< (abs (/ (- guess guess_prev)
             guess_prev))
     0.001))


; Ex 1.8 - Newton's method for cube roots
(define (cbrt-iter guess x)
  (cond ((good-enough guess x) guess)
        (else (cbrt-iter (improve guess x) x))))
(define (cbrt x) (cbrt-iter 1.0 x))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)
(define (good-enough guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (square x) (* x x))
(define (cube x) (* (square x) x))


; 1.1.8 - Block structure
(define (sqrt x)
  (define (good-enough guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (cond ((good-enough guess x) guess)
          (else (sqrt-iter (improve guess x) x))))
  (sqrt-iter 1.0 x))


; 1.1.8 - Lexical scoping, inner functions will take parameter values from
; their nearest scope as long as another parameter of the same name is
; not bound in their definition.
(define (sqrt x)
  (define (good-enough guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (cond ((good-enough guess x) guess)
          (else (sqrt-iter (improve guess x) x))))
  (sqrt-iter 1.0 x))
