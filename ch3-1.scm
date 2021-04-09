#!/usr/bin/env -S guile -s
!#

(define fail-count 0)

(define (print message . args)
  (apply simple-format #t message args))

(define (test x y)
  (if ((if (number? x) = equal?) x y)
    (print "~A\n" 'pass)
    (begin
      (print "fail: expected ~A got ~A\n" y x)
      (set! fail-count (+ fail-count 1)))))

(define (run-test name)
  (print "\n")
  (print "~A\n" name)
  ((eval name (interaction-environment))))

(define (square x) (* x x))

(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x))
    n))

(define (ex3-1)
  (define A (make-accumulator 5))
  (test (A 10) 15)
  (test (A 10) 25))

(define (make-monitored f)
  (let ((count 0))
   (lambda (v)
     (cond ((eq? v 'how-many-calls?) count)
           ((eq? v 'reset-count) (set! count 0))
           (else
             (set! count (+ count 1))
             (f v))))))

(define (ex3-2)
  (define s (make-monitored sqrt))
  (test (s 100) 10)
  (test (s 'how-many-calls?) 1)
  (s 'reset-count)
  (test (s 'how-many-calls?) 0))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      (lambda (v) "Incorrect password")))
  dispatch)


(define (ex3-3)
  (define acc (make-account 100 'secret-password))
  (test ((acc 'secret-password 'withdraw) 40) 60)
  (test ((acc 'some-other-password 'deposit) 50) "Incorrect password"))

(define cops-called #f)
(define (call-the-cops)
  (set! cops-called #t))

(define (make-account-1 balance password)
  (let ((fail-count 0))
   (define (withdraw amount)
     (if (>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (define (dispatch p m)
     (if (eq? p password)
       (begin
         (set! fail-count 0)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request -- MAKE-ACCOUNT" m))))
       (lambda (v)
         (set! fail-count (+ fail-count 1))
         (if (> fail-count 7) (call-the-cops))
         "Incorrect password")))
   dispatch))

(define (run-n f args n)
  (if (> n 0)
    (begin
      (apply f args)
      (run-n f args (- n 1)))))

(define (ex3-4)
  (define acc (make-account-1 100 'secret-password))
  (test ((acc 'secret-password 'withdraw) 40) 60)
  (test ((acc 'some-other-password 'deposit) 50) "Incorrect password")
  (test cops-called #f)
  (run-n (acc 'some-other-password 'deposit) '(50) 7)
  (test cops-called #t))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (+ low (random (- high low))))


(define (estimate-integral P x1 x2 y1 y2)
  (let ((area (* (- x2 x1) (- y2 y1)))
        (experiment (lambda () (P (random-in-range x1 x2)
                                  (random-in-range y1 y2)))))
    (exact->inexact (* area (monte-carlo 100000 experiment)))))

(define (ex3-5)
  (define (pred x y) (>= (+ (* x x) (* y y)) 1.0))
  ;; this method can have random results and is not even close to pi,
  ;; normally ~3
  (test (number? (estimate-integral pred -1 1 -1 1)) #t))

(define random-init 0)
(define (rand-update x) (+ x 1))

(define rand
  (let ((v random-init))
   (define (dispatch message)
     (cond ((eq? message 'generate)
            (set! v (rand-update v))
            v)
           ((eq? message 'reset)
            (lambda (n) (set! v n)))
           (error "Unknown request -- RAND" message)))
   dispatch))

(define (ex3-6)
  ((rand 'reset) 0)
  (let ((first (rand 'generate)))
   ((rand 'reset) 0)
   (let ((second (rand 'generate)))
    (test first second))))

(define (make-joint account orig-pass new-pass)
  (define (dispatch password message)
    (if (eq? password new-pass)
      (account orig-pass message)
      "Incorrect password"))
  dispatch)

(define (ex3-7)
  (define paul-acc (make-account 100 'open-sesame))
  (define peter-acc (make-joint paul-acc 'open-sesame 'rosebud))

  (test ((paul-acc 'open-sesame 'withdraw) 10) 90)
  (test ((peter-acc 'rosebud 'withdraw) 10) 80))

(define (ex3-8)
  (define last #f)
  (define (f v)
    (if (not last) (set! last v))
    last)

  ;; left-to-right evaluation
  (test (+ (f 0) (f 1)) 0)
  )

(for-each run-test '(ex3-1 ex3-2 ex3-3 ex3-4 ex3-5 ex3-6 ex3-7 ex3-8))

(exit fail-count)
