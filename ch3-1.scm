
;; 3.1.1 - Local state variables

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (raise (list "Unknown request -- MAKE-ACCOUNT" m))))))

;; Ex 3.1
(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

;; Ex 3.2
(define (make-monitored f)
  (let ((times 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) times)
            ((eq? arg 'reset-count) (set! times 0))
            (else (begin (set! times (+ times 1))
                         (f arg)))))))

;; Ex 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (lambda (pass m)
    (cond ((not (eq? pass password)) (lambda (x) "Incorrect Password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (raise (list "Unknown request -- MAKE-ACCOUNT" m))))))

;; Ex 3.4
(define (make-account balance password)
  (define (call-the-cops) "Call the cops.")
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-pass-count 0))
    (lambda (pass m)
      (if (not (eq? pass password))
          (lambda (x)
            (if (>= wrong-pass-count 4)
                (call-the-cops)
                (begin (set! wrong-pass-count (+ wrong-pass-count 1))
                       "Incorrect password.")))
          (begin (set! wrong-pass-count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (raise (list "Unknown request -- MAKE-ACCOUNT"
                                          m)))))))))


;; 3.1.2 - Benefits of introducing assignment
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Ex 3.6
(define rand
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? message 'reset)
             (lambda (newx) (set! x newx)))))))

