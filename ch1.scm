
; 1.3
(define (sum2sq a b) (+ (* a a) (* b b)))
(define (bothlargerthan a b c) (and (> a c) (> b c)))
(define (ss2 a b c)
    (cond
        ((bothlargerthan a b c) (sum2sq a b))
        ((bothlargerthan a c b) (sum2sq a c))
        ((bothlargerthan b c a) (sum2sq b c))
    )
)
