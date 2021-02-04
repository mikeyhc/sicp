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
    (sqrt-iter (improve guess x) x)))

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

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (ex1-29)
  (define (cube x) (* x x x))

  (define (integral f a b dx)
    (define (add-dx x) (+ x dx))

    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

  (define (inc x) (+ 1 x))

  (define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* h k))))
    (define (s k)
      (* (cond ((= k 0) 1)
               ((= k n) 1)
               ((even? k) 2)
               (else 4))
         (y k)))

    (* (/ h 3) (sum s 0 inc n)))

  (print "~A\n" (integral cube 0 1 0.01))
  (print "~A\n" (integral cube 0 1 0.001))
  (print "~A\n" (simpson cube 0 1 100))
  (print "~A\n" (simpson cube 0 1 1000)))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))

  (iter a 0))

(define (ex1-30)
  (define (cube x) (* x x x))
  (define (inc x) (+ x 1))

  (test (sum-iter cube 1 inc 10) (sum cube 1 inc 10)))

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))

  (iter a 1))

(define (ex1-31)
  (define (inc x) (+ x 1))

  (define (pi-f n)
    (define num
      (+ 2 (if (odd? n) (- n 1) n)))
    (define denom
      (+ 2 (if (even? n) (- n 1) n)))

    (/ num denom))

  (test (product (lambda (x) x) 1 inc 10) 3628800)
  (test (product-iter (lambda (x) x) 1 inc 10) 3628800)
  (print "~A\n" (exact->inexact (* 4 (product pi-f 1 inc 1000)))))

(define (accumulate combiner null term a next b)
  (if (> a b)
    null
    (combiner (term a)
              (accumulate combiner null term (next a) next b))))

(define (accumulate-iter combiner null term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))

  (iter a null))

(define (ex1-32)
  (define (inc x) (+ x 1))
  (define (cube x) (* x x x))

  (define (sum-acc term a next b)
    (accumulate + 0 term a next b))

  (define (product-acc term a next b)
    (accumulate * 1 term a next b))

  (define (sum-acc-iter term a next b)
    (accumulate-iter + 0 term a next b))

  (define (product-acc-iter term a next b)
    (accumulate-iter * 1 term a next b))

  (test (sum cube 1 inc 10) (sum-acc cube 1 inc 10))
  (test (product-acc cube 1 inc 10) (product cube 1 inc 10))
  (test (sum-iter cube 1 inc 10) (sum-acc-iter cube 1 inc 10))
  (test (product-acc-iter cube 1 inc 10) (product-iter cube 1 inc 10)))

(define (filtered-accumulate combiner filter null term a next b)
  (if (> a b)
    null
    (combiner
      (if (filter a) (term a) null)
      (filtered-accumulate combiner filter null term (next a) next b))))

(define (ex1-33)
  (define (inc x) (+ x 1))
  (define (square x) (* x x))

  (define (sum-sq-primes a b)
    (filtered-accumulate + prime? 0 square a inc b))

  (define (rel-prime-prod n)
    (define (rel-prime? x) (= (gcd x n) 1))
    (define (id x) x)

    (filtered-accumulate * rel-prime? 1 id 1 inc (- n 1)))

  (test (sum-sq-primes 2 10) 87)
  (test (rel-prime-prod 9) 2240))

;; ex1-34
;; (define (f g) (g 2))
;; (f f)
;; (f 2)
;; (2 2)
;; error (in guile this will hang the REPL)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (let ((next (f guess)))
     (if (close-enough? guess next)
       next
       (try next))))

  (try first-guess))

(define (ex1-35)
  (define gr (/ (+ 1 (sqrt 5)) 2))

  (define (grt x)
    (+ 1 (/ 1 x)))

  (test #t (< (abs (- (fixed-point grt 1.0) gr)) tolerance)))

(define (ex1-36)
  (define (noisy-fixed-point f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))

    (define (try guess)
      (let ((next (f guess)))
       (display guess)
       (newline)
       (if (close-enough? guess next)
         next
         (try next))))

    (try first-guess))

  (define (log-over-log x) (/ (log 1000) (log x)))
  (define (avg-log-over-log x)
    (average x (/ (log 1000) (log x))))

  (print "~A\n" (noisy-fixed-point log-over-log 2))
  (newline)
  (print "~A\n" (noisy-fixed-point avg-log-over-log 2))
  )

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (ex1-37)
  (define gr (/ (+ 1 (sqrt 5)) 2))
  (test (< (abs (- (cont-frac (lambda (i) 1.0)
                             (lambda (i) 1.0)
                             11
                             )
                  (/ 1 gr))) 0.0001) #t)
  )

(define (ex1-38)
  (define (e-f x)
    (cond ((= x 2) x)
          ((< x 5) 1)
          ((= (remainder (- x 5) 3) 0)
           (- x (/ (- x 2) 3)))
          (else 1)))

  (print "~A\n" (+ (cont-frac (lambda (i) 1.0) e-f 20) 2)))

(define (ex1-39)
  (define (tan-cf x k)
    (cont-frac (lambda (i) (if (= i 1) x (-  (* x x))))
               (lambda (i) (- (* i 2) 1))
               k))

  (test (tan 0.5) (tan-cf 0.5 10)))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (ex1-40)
  (define (cube x) (* x x x))
  (define (sq x) (* x x))
  (define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (sq x) (* b x) c))))

  (print "~A\n" (newtons-method (cubic 0 0 0) 1)))

(define (ex1-41)
  (define (double f)
    (lambda (x) (f (f x))))
  (define (inc x) (+ 1 x))

  (test ((double inc) 0) 2)
  (test (((double (double double)) inc) 5) 21))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (ex1-42)
  (test ((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6) 49))

(define (repeated f n)
  (if (<= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (ex1-43)
  (test ((repeated (lambda (x) (* x x)) 2) 5) 625))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (ex1-44)
  (define (cube x) (* x x x))
  (print "~A\n" ((n-smooth cube 1) 5))
  (print "~A\n" ((n-smooth cube 2) 5))
  (print "~A\n" ((n-smooth cube 3) 5))
  )

(define (ex1-45)
  (define (average-damp f)
    (lambda (x) (average x (f x))))

  (define (get-max-pow n)
    (define (iter p r)
      (if (< (- n r) 0)
        (- p 1)
        (iter (+ p 1) (* r 2))))
    (iter 1 2))

  (define (sq x) (* x x))

  (define (pow b p)
    (define (iter res a n)
      (cond ((= n 0) res)
            ((even? n) (iter res (sq a) (/ n 2)))
            (else (iter (* res a) a (- n 1)))))
    (iter 1 b p))

  (define (nth-root n x)
    (fixed-point ((repeated average-damp (get-max-pow n))
                  (lambda (y) (/ x (pow y (- n 1)))))
                 1.0))

  (test (nth-root 5 32) 2.000001512995761))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve) (improve guess)))))

(define (ex1-46)
  (define (fixed-point-ii f guess)
    ((iterative-improve
       (lambda (x) (< (abs (- x (f x))) tolerance))
       f)
     guess))

  (define (sqrt-ii x)
    ((iterative-improve
       (lambda (y) (< (abs (- (* y y) x)) 0.0001))
       (lambda (y) (average y (/ x y))))
       1.0))

  (define (cube x) (* x x x))

  (test (fixed-point cube 1.0) (fixed-point-ii cube 1.0))
  (test (< (abs (- (sqrt 25) (sqrt-ii 25))) 0.0001) #t))

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
(for-each run-test '(ex1-27 ex1-28 ex1-29 ex1-30 ex1-31 ex1-32 ex1-33))
(print "\nex1-34 omitted\n")
(for-each run-test '(ex1-35 ex1-36 ex1-37 ex1-38 ex1-39 ex1-40 ex1-41 ex1-42
                            ex1-43 ex1-44 ex1-45 ex1-46))

(exit fail-count)
