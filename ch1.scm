#!/usr/bin/env -S csi -s

(define fail-count 0)

(define (test x y)
  (if ((if (number? x) = equal?) x y)
    (print 'pass)
    (begin
      (print "fail: expected " y " got " x)
      (set! fail-count (+ fail-count 1)))))

(define (run-test name)
  (print)
  (print name)
  ((eval name)))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex1-1)
  (test 10 10)
  (test (+ 5 3 4) 12)
  (test (- 9 1) 8)
  (test (/ 6 2) 3)
  (define a 3)
  (define b (+ a 1))
  (test (+ a b (* a b)) 19)
  (test (= a b) #f)
  (test (if (and (> b a) (< b (* a b))) b a) b)
  (test (cond ((= a 4) 6)
              ((= b 4) (+ 6 7 a))
              (else 25)) (+ 6 7 a))
  (test (+ 2 (if (> b a) b a)) (+ 2 b))
  (test (* (cond ((> a b) a)
                 ((< a b) b)
                 (else -1))
           (+ a 1)) (* b (+ a 1))))

(define (ex1-2)
  (define f (/ (+ 5 4 (- 2 (- 3 ( + 6 (/ 4 5)))))
               (* 3 (- 6 2) (- 2 7))))
  (test f (/ (+ 14 (/ 4 5)) -60)))

(define (ex1-3)
  (define (f x y z)
    (cond ((and (> x z) (> y z)) (sum-of-squares x y))
          ((> x y) (sum-of-squares x z))
          (else (sum-of-squares y z))))
  (test (f 1 2 3) 13)
  (test (f 3 2 1) 13)
  (test (f 1 3 2) 13)
  (test (f 2 3 1) 13)
  (test (f 2 2 2) 8)
  (test (f 0 0 0) 0))

(define (ex1-4)
  (define (a-plus-abs-b a b)
    ;; this function with return a + the absolute value of b
    ((if (> b 0) + -) a b))

  (test (a-plus-abs-b 3 4) 7)
  (test (a-plus-abs-b 3 -4) 7))


;; WARNING: do not run, will infinite loop
(define (ex1-5)
  (define (p) (p))
  (define (test x y)
    (if (= x 0) 0 y))
  ;; if applicative order evaluation is used then p is evaluated and never
  ;; returns
  ;;
  ;; if normal order evaluation is used then p is never evaluated and function
  ;; returns 0
  (test 0 (p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqrt functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (square-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end sqrt functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: do not run, will infinite loop
(define (ex1-6)
  (define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))

  (define (sqrt-iter guess x)
    ;; as this is not a special form all arguments are evaluated
    (new-if (good-enough? guess x)
            guess
            ;; this will recurse forever
            (sqrt-iter (improve guess x) x))))

(define (ex1-7)
  (define (good-enough? guess last)
    (< (abs (- guess last)) 0.00001))
  (define (sqrt-iter guess x last)
    (if (good-enough? guess last)
      guess
      (sqrt-iter (improve guess x) x guess)))
  (define (sqrt x)
    (sqrt-iter 1.0 x 0.0))
  (define chicken-sqrt-2 1.4142135623731)
  (test (< (abs (- (sqrt 2.0) chicken-sqrt-2)) 0.00001) #t))

(for-each run-test '(ex1-1 ex1-2 ex1-3 ex1-4))
(print "ex1-5 omitted")
(print "ex1-6 omitted")
(run-test 'ex1-7)

(exit fail-count)
