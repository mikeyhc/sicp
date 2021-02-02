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

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (ex1-1)
  (define a 3)
  (define b (+ a 1))

  (test 10 10)
  (test (+ 5 3 4) 12)
  (test (- 9 1) 8)
  (test (/ 6 2) 3)
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
  (define f (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
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
            (sqrt-iter (improve guess x) x)))
  #f)

(define (ex1-7)
  ;; very small numbers will may have a value below the tolerance
  ;; very large numbers may exceed the precision of the tolerance
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

(define (ex1-8)
  (define (good-enough? guess last)
    (< (abs (- guess last)) 0.00001))

  (define (cube-iter guess x last)
    (if (good-enough? guess last)
      guess
      (cube-iter (improve guess x) x guess)))

  (define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (define (cbrt x)
    (cube-iter 1.0 x 0.0))

  (define google-cbrt-2 1.25992104989)
  (test (< (abs (- (cbrt 2.0) google-cbrt-2)) 0.00001) #t))

(define (ex1-9)
  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))

  ;; recursive process
  ;; (f+ 4 5)
  ;; (inc (f+ 3 5))
  ;; (inc (inc (f+ 2 5)))
  ;; (inc (inc (inc (f+ 1 5))))
  ;; (inc (inc (inc (inc (f+ 0 5)))))
  ;; (inc (inc (inc (inc 5))))
  ;; (inc (inc (inc 6)))
  ;; (inc (inc 7))
  ;; (inc 8)
  ;; 9
  (define (f+ a b)
    (if (= a 0)
      b
      (inc (f+ (dec a) b))))

  ;; iterative process
  ;; (g+ 4 5)
  ;; (g+ (dec 4) (inc 5))
  ;; (g+ (dec 3) (inc 6))
  ;; (g+ (dec 2) (inc 7))
  ;; (g+ (dec 1) (inc 8))
  ;; 9
  (define (g+ a b)
    (if (= a 0)
      b
      (g+ (dec a) (inc b))))

  (test (f+ 4 5) (g+ 4 5)))

(define (ex1-10)
  (define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))))

  ;; computes 2n
  (define (f n) (A 0 n))
  ;; computes 2^n if n > 0
  (define (g n) (A 1 n))
  ;; computes 2^2^(n - 1 times) for n > 1
  (define (h n) (A 2 n))
  ;; computes 5n^2
  (define (k n) (* 5 n n))

  (test (f 2) 4)
  (test (g 2) 4)
  (test (h 2) 4)
  (test (k 2) 20)
  )

(for-each run-test '(ex1-1 ex1-2 ex1-3 ex1-4))
(print "\n")
(for-each print '("ex1-5 omitted\n"
                  "ex1-6 omitted\n"))
(for-each run-test '(ex1-7 ex1-8 ex1-9 ex1-10))

(exit fail-count)
