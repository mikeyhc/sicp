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
  (test (k 2) 20))

(define (ex1-11)
  ;; recursive process
  (define (f n)
    (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

  (define (f-iter n)
    ; a = f(n - 1)
    ; b = f(n - 2)
    ; c = f(n - 3)
    (define (iter a b c count)
      (if (= count n)
        a
        (iter
          (+ a (* b 2) (* c 3)) a b (+ count 1))))

    (if (< n 3)
      n
      (iter 2 1 0 2)))

  (for-each (lambda (x) (test (f x) (f-iter x))) '(4 5 6 7 8)))

(define (ex1-12)
  (define (triangle-number x y)
    (cond ((< x 1) 0)
          ((> x y) 0)
          ((= y 1) 1)
          (else
            (+ (triangle-number (- x 1) (- y 1))
               (triangle-number x (- y 1))))))

  (test (triangle-number 1 1) 1)
  (test (triangle-number 1 2) 1)
  (test (triangle-number 2 2) 1)
  (test (triangle-number 1 3) 1)
  (test (triangle-number 2 3) 2)
  (test (triangle-number 3 3) 1)
  (test (triangle-number 1 4) 1)
  (test (triangle-number 2 4) 3)
  (test (triangle-number 3 4) 3)
  (test (triangle-number 4 4) 1)
  (test (triangle-number 1 5) 1)
  (test (triangle-number 2 5) 4)
  (test (triangle-number 3 5) 6)
  (test (triangle-number 4 5) 4)
  (test (triangle-number 5 5) 1))

;; ex1-13 and ex1-14 were performed on paper

(define (ex1-15)
  (define p-call 0)

  (define (cube x) (* x x x))

  (define (p x)
    (set! p-call (+ p-call 1))
    (- (* 3 x) (* 4 (cube x))))

  (define (sine angle)
    (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

  (sine 12.15)
  ;; a
  (test p-call 5)
  ;; b
  ;; SpaceO(sine a) = O(log(a))
  ;; StepsO(sine a) = O(log(a))
  )

(define (ex1-16)
  (define (fast-expt b n)
    (define (iter i j a)
      (cond ((= i 0) a)
            ((even? i) (iter (/ i 2) (* j j) a))
            (else (iter (- i 1) j (* a j)))))

    (iter n b 1))

  (test (fast-expt 2 2) (expt 2 2))
  (test (fast-expt 3 3) (expt 3 3))
  (test (fast-expt 4 4) (expt 4 4))
  (test (fast-expt 5 5) (expt 5 5)))

(define (ex1-17)
  (define (double n) (* n 2))
  (define (halve n) (/ n 2))

  (define (fast-* a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-* a (halve b))))
          (else (+ a (fast-* a (- b 1))))))

  (test (fast-* 2 2) (* 2 2))
  (test (fast-* 3 3) (* 3 3))
  (test (fast-* 4 4) (* 4 4))
  (test (fast-* 5 5) (* 5 5))
  (test (fast-* 6 6) (* 6 6)))

(define (ex1-18)
  (define (double n) (* n 2))
  (define (halve n) (/ n 2))

  (define (fast-* a b)
    (define (iter x y r)
      (cond ((= y 0) r)
            ((even? y) (iter (double x) (halve y) r))
            (else (iter x (- y 1) (+ x r)))))
    (iter a b 0))

  (test (fast-* 2 2) (* 2 2))
  (test (fast-* 3 3) (* 3 3))
  (test (fast-* 4 4) (* 4 4))
  (test (fast-* 5 5) (* 5 5))
  (test (fast-* 6 6) (* 6 6)))

(define (ex1-19)
  (define (fib n)
    (fib-iter 1 0 0 1 n))

  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

  (test (fib 1) 1)
  (test (fib 2) 1)
  (test (fib 3) 2)
  (test (fib 4) 3)
  (test (fib 5) 5)
  (test (fib 6) 8))

;; ex1-20 performed on paper

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (ex1-21)
  (test (smallest-divisor 199) 199)
  (test (smallest-divisor 1999) 1999)
  (test (smallest-divisor 19999) 7))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (get-internal-run-time)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (get-internal-run-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (ex1-22)
  (define (find-smallest-primes a b)
    (if (< a b)
      (begin
        (timed-prime-test a)
        (find-smallest-primes (+ a 1) b))))

  (timed-prime-test 1009)
  (timed-prime-test 1013)
  (timed-prime-test 1019)
  (timed-prime-test 10007)
  (timed-prime-test 10009)
  (timed-prime-test 10037)
  (timed-prime-test 100003)
  (timed-prime-test 100019)
  (timed-prime-test 100043)
  (timed-prime-test 1000003)
  (timed-prime-test 1000033)
  (timed-prime-test 1000037)
  (newline))

(define (ex1-23)
  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (prime? n)
    (= n (smallest-divisor n)))

  (define (next n)
    (if (= n 2) 3 (+ n 2)))

  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (get-internal-run-time)))

  (define (start-prime-test n start-time)
    (if (prime? n)
      (report-prime (- (get-internal-run-time) start-time))))

  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  ; the if test means that the speedup is not exactly 2, closer to 1.5
  (timed-prime-test 1009)
  (timed-prime-test 1013)
  (timed-prime-test 1019)
  (timed-prime-test 10007)
  (timed-prime-test 10009)
  (timed-prime-test 10037)
  (timed-prime-test 100003)
  (timed-prime-test 100019)
  (timed-prime-test 100043)
  (timed-prime-test 1000003)
  (timed-prime-test 1000033)
  (timed-prime-test 1000037)
  (newline))

(define (ex1-24)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))

  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

  (define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (get-internal-run-time)))

  (define (start-prime-test n start-time)
    (if (fast-prime? n 1)
      (report-prime (- (get-internal-run-time) start-time))))

  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  ; there is overhead incurred by the complexity of processing large numbers
  (timed-prime-test 1009)
  (timed-prime-test 1013)
  (timed-prime-test 1019)
  (timed-prime-test 10007)
  (timed-prime-test 10009)
  (timed-prime-test 10037)
  (timed-prime-test 100003)
  (timed-prime-test 100019)
  (timed-prime-test 100043)
  (timed-prime-test 1000003)
  (timed-prime-test 1000033)
  (timed-prime-test 1000037)
  (newline))

;; ex1-25
;; using a regular expt would result in large numbers and slower processing

;; ex1-26
;; this layour results to 2 calls to expmod instead of 2, therefore increasing
;; the calls from log(n) to n

(define (ex1-27)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m))
                      m))
          (else
            (remainder (* base (expmod base (- exp 1) m))
                       m))))

  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

  (define (test-carmichael n)
    (define (iter a n)
      (cond ((= a n) #t)
            ((= (expmod a n n) a) (iter (+ a 1) n))
            (else #f)))
    (iter 1 n))

  (test (test-carmichael 561) #t)
  (test (test-carmichael 1105) #t)
  (test (test-carmichael 1729) #t)
  (test (test-carmichael 2465) #t)
  (test (test-carmichael 2821) #t)
  (test (test-carmichael 6601) #t))

(define (ex1-28)

  (define (mr-expmod base exp m)
    (define (squaremod-with-check x)
      (define (check-nontrivial-sqrt1 x square)
        (if (and (= square 1)
                 (not (= x 1))
                 (not (= x (- m 1))))
          0
          square))
      (check-nontrivial-sqrt1 x (remainder (square x) m)))

    (cond ((= exp 0) 1)
          ((even? exp) (squaremod-with-check
                         (mr-expmod base (/ exp 2) m)))
          (else
            (remainder (* base (mr-expmod base (- exp 1) m)) m))))

  (define (miller-rabin-test n)
    (define (try-it a)
      (define (check-it x)
        (and (not (= x 0)) (= x 1)))
      (check-it (mr-expmod a (- n 1) n)))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else #f)))

  (test (fast-prime? 561 1) #f)
  (test (fast-prime? 1105 1) #f)
  (test (fast-prime? 1729 1) #f)
  (test (fast-prime? 2465 1) #f)
  (test (fast-prime? 2821 1) #f)
  (test (fast-prime? 6601 1) #f))

(for-each run-test '(ex1-1 ex1-2 ex1-3 ex1-4))
(print "\n")
(for-each print '("ex1-5 omitted\n"
                  "ex1-6 omitted\n"))
(for-each run-test '(ex1-7 ex1-8 ex1-9 ex1-10 ex1-11 ex1-12))
(print "\n")
(for-each print '("ex1-13 omitted\n"
                  "ex1-14 omitted\n"))
(for-each run-test '(ex1-15 ex1-16 ex1-17 ex1-18 ex1-19))
(print "\nex1-20 omitted\n")
(for-each run-test '(ex1-21 ex1-22 ex1-23 ex1-24))
(for-each print '("ex1-25 omitted\n"
                  "ex1-26 omitted\n"))
(for-each run-test '(ex1-27 ex1-28))

(exit fail-count)
