#!/usr/bin/env -S guile -s
!#

(define fail-count 0)

(define (print message . args)
  (apply simple-format #t message args))

(define (test x y)
  (if (equal? x y)
    (print "~A\n" 'pass)
    (begin
      (print "fail: expected ~A got ~A\n" y x)
      (set! fail-count (+ fail-count 1)))))

(define (run-test name)
  (print "\n")
  (print "~A\n" name)
  ((eval name (interaction-environment))))

(define (make-rat n d)
  (let ((g (gcd n d)))
   (if (and (>= n 0) (< d 0))
     (cons (/ (- 0 n) g) (/ (- 0 d) g))
     (cons (/ n g) (/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (ex2-1)
  (let ((half (make-rat 1 2))
        (other-half (make-rat 2 4))
        (neg-half (make-rat -2 4))
        (other-neg-half (make-rat 2 -4)))
    (test (numer half) 1)
    (test (denom half) 2)
    (test (numer other-half) 1)
    (test (denom other-half) 2)
    (test (numer neg-half) -1)
    (test (denom neg-half) 2)
    (test (numer other-neg-half) -1)
    (test (denom other-neg-half) 2)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (define (average a b) (/ (+ a b) 2))

  (cons (average (x-point (start-segment segment))
                 (x-point (end-segment segment)))
        (average (y-point (start-segment segment))
                 (y-point (end-segment segment)))))

(define (ex2-2)
  (let ((midpoint (midpoint-segment
                    (make-segment (make-point 0 0) (make-point 10 10)))))
    (test (x-point midpoint) 5)
    (test (y-point midpoint) 5)))

(define (make-rect top-left bottom-right)
  (cons
    (abs (- (x-point top-left) (x-point bottom-right)))
    (cons
      (abs (- (y-point top-left) (y-point bottom-right)))
      (cons top-left bottom-right))))

(define (rect-top-left r)
  (caddr r))

(define (rect-bottom-right r)
  (cdddr r))

(define (rect-width r)
  (car r))

(define (rect-height r)
  (cadr r))

(define (rect-perimeter r)
  (+ (* 2 (rect-width r))
     (* 2 (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

(define (make-orect segment)
  (cons
    (abs (- (x-point (start-segment segment))
            (x-point (end-segment segment))))
    (cons
      (abs (- (y-point (start-segment segment))
              (y-point (end-segment segment))))
      segment)))

(define (ex2-3)
  (let ((unit-rect (make-rect (make-point 0 0) (make-point 1 1)))
        (unit-orect (make-orect (make-segment (make-point 0 0)
                                              (make-point 1 1)))))
    (test (rect-perimeter unit-rect) 4)
    (test (rect-area unit-rect) 1)
    (test (rect-perimeter unit-orect) 4)
    (test (rect-area unit-orect) 1)))

(define (ex2-4)
  (define (cons x y)
    (lambda (m) (m x y)))

  (define (car z)
    (z (lambda (p q) p)))

  (define (cdr z)
    (z (lambda (p q) q)))

  (test (car (cons 1 2)) 1)
  (test (cdr (cons 1 2)) 2))

(define (ex2-5)
  (define (remainder-divisions n p d)
    (if (= (remainder p d) 0)
      (remainder-divisions (+ n 1) (/ p d) d)
      n))

  (define (cons a b)
    (* (expt 2 a) (expt 3 b)))

  (define (car c)
    (remainder-divisions 0 c 2))

  (define (cdr c)
    (remainder-divisions 0 c 3))

  (test (car (cons 1 2)) 1)
  (test (cdr (cons 1 2)) 2))

(define (ex2-6)
  (define zero (lambda (f) (lambda (x) x)))

  (define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

  (define one
    (lambda (f) (lambda (x) (f x))))

  (define two
    (lambda (f) (lambda (x) (f (f x)))))

  (define (add a b)
    (lambda (f)
      (lambda (x)
        ((a f) ((b f) x)))))

  (define (church->int cn)
    ((cn (lambda (n) (+ n 1))) 0))

  (test (church->int zero) 0)
  (test (church->int one) 1)
  (test (church->int two) 2)
  (test (church->int (add one two)) 3))

(define (make-interval a b)
  (if (< a b)
    (cons a b)
    (cons b a)))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (ex2-7)
  (test (lower-bound (make-interval 0 10)) 0)
  (test (upper-bound (make-interval 0 10)) 10))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (let ((i1 (- (lower-bound x) (lower-bound y)))
        (i2 (- (lower-bound x) (upper-bound y)))
        (i3 (- (upper-bound x) (lower-bound y)))
        (i4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min i1 i2 i3 i4) (max i1 i2 i3 i4))))

(define (mul-interval x y)
  (define (opposite-pair? a b)
    (if (positive? a)
      (negative? b)
      (positive? b)))

  (define (positive-pair? a b)
    (if (opposite-pair? a b)
      #f
      (positive? a)))

  (define (negative-pair? a b)
    (if (opposite-pair? a b)
      #f
      (negative? a)))

  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond ((negative-pair? x0 x1)
           (cond ((opposite-pair? y0 y1)
                  (make-interval (* x0 y0) (* x0 y1)))
                 ((negative-pair? y0 y1)
                  (make-interval (* x1 y1) (* x0 y0)))
                 (else
                   (make-interval (* x1 y0) (* x0 y1)))))
          ((positive-pair? x0 x1)
           (cond ((opposite-pair? y0 y1)
                  (make-interval (* x1 y0) (* x1 y1)))
                 ((negative-pair? y0 y1)
                  (make-interval (* x1 y0) (* x0 y1)))
                 (else
                   (make-interval (* x0 y0) (* x1 y1)))))
          (else
            (cond ((positive-pair? y0 y1)
                   (make-interval (* x0 y1) (* x1 y1)))
                  ((negative-pair? y0 y1)
                   (make-interval (* x1 y0) (* x0 y0)))
                  (else
                    (make-interval
                      ((lambda (a b) (if (< a b) a b)) (* x0 y1) (* x1 y0))
                      ((lambda (a b) (if (> a b) a b))
                       (* x0 y0) (* x1 y1)))))))))

(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0)
          (= (lower-bound y) (upper-bound y)))
    #f
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define (ex2-8)
  (let ((i (sub-interval (make-interval 5 10) (make-interval 1 5))))
   (test (lower-bound i) 0)
   (test (upper-bound i) 9)))

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (ex2-9)
  (let ((i0 (make-interval 0 5))
        (i1 (make-interval 3 7)))

    (test (+ (width-interval i0) (width-interval i1))
          (width-interval (add-interval i0 i1)))
    (test (= (* (width-interval i0) (width-interval i1))
             (width-interval (mul-interval i0 i1))) #f)))

(define (ex2-10)
  (let ((i0 (make-interval 0 5))
        (i1 (make-interval -5 0))
        (i2 (make-interval 5 5)))
    (test (div-interval i0 i1) #f)
    (test (div-interval i1 i0) #f)
    (test (div-interval i0 i2) #f)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percentage c p)
  (let ((w (* c p)))
   (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percentage i)
  (/ (width i) (center i)))

(define (ex2-12)
  (test (< (abs (- (percentage (make-center-percentage 3.5 0.15)) 0.15))
           0.00001) #t)
  )

(define ex2-13 ex2-12)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
   (div-interval one
                 (add-interval (div-interval one r1)
                               (div-interval one r2)))))

(define (ex2-14)
  (let ((i1 (make-center-percentage 5 0.02))
        (i2 (make-center-percentage 7 0.05)))
    (test (eq? (par1 i1 i2) (par2 i1 i2)) #f)
    )
  )

;; ex2-15
;; The issue is due to the fact that ranges operate over a range so have no
;; fixed identity, so each time a range appears in the formula it will introduce
;; more uncertainty. par2 is better than par1 because of this reason.

;; ex2-16
;; I don't have time to prove this, but my gut says it isn't possible without an
;; alternate representation of intervals that have an identity.

(define nil '())
(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7))

(define (last-pair l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(define (ex2-17)
  (test (last-pair (list 23 72 149 34)) (list 34)))

(define (reverse l)
  (define (iter i acc)
    (if (null? i)
      acc
      (iter (cdr i) (cons (car i) acc))))

  (iter l nil))

(define (ex2-18)
  (test (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define no-more? null?)
  (define except-first-denomination cdr)
  (define first-denomination car)

  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (ex2-19)
  ; (test (cc 100 uk-coins) 104561)
  (test (cc 100 us-coins) 292))

(define (same-parity x . l)
  (define parity? (if (even? x) even? odd?))

  (define (iter l)
    (cond ((null? l) l)
          ((parity? (car l)) (cons (car l) (iter (cdr l))))
          (else (iter (cdr l)))))

  (cons x (iter l)))

(define (ex2-20)
  (test (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
  (test (same-parity 2 3 4 5 6 7) (list 2 4 6)))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (ex2-21)
  (test (square-list (list 1 2 3 4)) (list 1 4 9 16)))

;; ex2-22
;; in the first example things are prepended to the result as they are
;; encountered which reverses the list. reversing the arguments to cons
;; doesn't fix it (and creates an improper list) because the values are
;; still encountered in the same order

(define (ex2-23)
  (define (for-each f l)
    (if (null? l)
      #t
      (begin
        (f (car l))
        (for-each f (cdr l)))))

  (for-each (lambda (x) (display x) (newline))
            (list 57 321 88)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (ex2-24)
  ;; [1][]->[+][/]
  ;;         |
  ;;         +->[2][]->[+][/]
  ;;                    |
  ;;                    +->[3][]->[4][/]
  ;;
  ;; +-> 1
  ;; |
  ;; +-+-> 2
  ;;   |
  ;;   +-+-> 3
  ;;     |
  ;;     +-> 4
  (define input (list 1 (list 2 (list 3 4))))

  (test (count-leaves input) 4)
  )

(define (ex2-25)
  (define l
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

  (test (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) 7)
  (test (car (car (list (list 7)))) 7)
  (test
    (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l))))))))))))
    7))

(define (ex2-26)
  (define x (list 1 2 3))
  (define y (list 4 5 6))

  (test (append x y) (list 1 2 3 4 5 6))
  (test (cons x y) (cons (list 1 2 3) (list 4 5 6)))
  (test (list x y) (list (list 1 2 3) (list 4 5 6))))

(define (deep-reverse l)
  (define (iter l acc)
    (cond ((null? l) acc)
          ((pair? (car l))
           (iter (cdr l) (cons (deep-reverse (car l)) acc)))
          (else (iter (cdr l) (cons (car l) acc)))))
  (iter l nil))

(define (ex2-27)
  (define x (list (list 1 2) (list 3 4)))
  (test (reverse x) (list (list 3 4) (list 1 2)))
  (test (deep-reverse x) (list (list 4 3) (list 2 1))))

(define (fringe t)
  (define (iter x acc)
    (cond ((null? x) acc)
          ((not (pair? x)) (cons x acc))
          (else (iter (cdr x) (iter (car x) acc)))))
  (reverse (iter t nil)))

(define (ex2-28)
  (define x (list (list 1 2) (list 3 4)))

  (test (fringe x) (list 1 2 3 4))
  (test (fringe (list x x)) (list 1 2 3 4 1 2 3 4)))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (define (get-weight branch)
    (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

  (+ (get-weight (left-branch mobile))
     (get-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (get-mobile-weight mobile)
    (+ (get-weight (left-branch mobile))
       (get-weight (right-branch mobile))))

  (define (get-weight branch)
    (* (branch-length branch)
       (if (pair? (branch-structure branch))
         (get-mobile-weight (branch-structure branch))
         (branch-structure branch))))

  (= (get-weight (left-branch mobile))
     (get-weight (right-branch mobile))))

(define (ex2-29)
  (define m (make-mobile (make-branch 1 4)
                         (make-branch 2 (make-mobile (make-branch 1 2)
                                                     (make-branch 2 1)))))
  (define bm (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                                      (make-branch 1 3)))
                          (make-branch 2 (make-mobile (make-branch 1 3)
                                                      (make-branch 1 2)))))
  ;; d. I would need to change the selector functions to the following
  ;; (define (right-branch mobile)
  ;;   (cdr mobile)
  ;;
  ;; (define (branch-structure branch)
  ;;   (cdr branch))

  (test (total-weight m) 7)
  (test (balanced? m) #f)
  (test (balanced? bm) #t))

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))

(define (ex2-30)
  (test (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
        (list 1 (list 4 (list 9 16) 25) (list 36 49))))

(define ex2-31 ex2-30)

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
     (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (ex2-32)
  ;; subsets works (in the non-trivial case) by splitting the problem. we
  ;; create 2 sets, the set containing all elements excluding x (called R),
  ;; and then all the sets including x (called S). The union of S + R contains
  ;; all possible subsets.
  (test (subsets (list 1 2 3))
        '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (ex2-33)
  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))

  (define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

  (define s1 (list 1 2 3 4))
  (define s2 (list 5 6 7 8))

  (test (map (lambda (x) (* x x)) s1) (list 1 4 9 16))
  (test (append s1 s2) (list 1 2 3 4 5 6 7 8))
  (test (length s1) 4))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (ex2-34)
  (test (horner-eval 2 (list 1 3 0 5 0 1)) 79))

(define (ex2-35)
  (define (count-leaves tree)
    (if (pair? tree)
      (accumulate (lambda (x acc)
                    (+ (if (pair? x)
                         (+ (count-leaves (car x))
                            (count-leaves (car (cdr x))))
                         1)
                       acc))
                  0
                  tree)
      1))

  (define input (list 1 (list 2 (list 3 4))))

  (test (count-leaves input) 4))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (ex2-36)
  (define s
    (list (list 1 2 3)
          (list 4 5 6)
          (list 7 8 9)
          (list 10 11 12)))

  (test (accumulate-n + 0 s) (list 22 26 30)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (accumulate + 0 (accumulate-n * 1 (list r v)))) m))

(define (transpose mat)
    (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
         (map (lambda (row)
                (map (lambda (col) (dot-product row col)) cols))
              m)))

(define (ex2-37)
  (define v1 (list 1 2 3 4))
  (define v2 (list 5 6 7 8))
  (define v3 (list 1 2 3))
  (define m1 (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)
                   (list 10 11 12)))
  (define m2 (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)))
  (define m3 (list (list 3 2 1)
                   (list 6 5 4)
                   (list 9 8 7)))

  (test (dot-product v1 v2) 70)
  (test (matrix-*-vector m1 v3) (list 14 32 50 68))
  (test (transpose m2) (list (list 1 4 7)
                             (list 2 5 8)
                             (list 3 6 9)))
  (test (matrix-*-matrix m2 m3) (list (list 42 36 30)
                                      (list 96 81 66)
                                      (list 150 126 102))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (ex2-38)
  ;; if op is transitive then fold-left and fold-right will
  ;; produce the same result
  (test (fold-right / 1 (list 1 2 3)) 3/2)
  (test (fold-left / 1 (list 1 2 3)) 1/6)
  (test (fold-right list nil (list 1 2 3))
        (list 1 (list 2 (list 3 nil))))
  (test (fold-left list nil (list 1 2 3))
        (list (list (list nil 1) 2) 3)))

(define (ex2-39)
  (define (reverse-r sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

  (define (reverse-l sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))

  (test (reverse-r (list 1 2 3)) (list 3 2 1))
  (test (reverse-l (list 1 2 3)) (list 3 2 1)))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (ex2-40)
  (test (unique-pairs 3) (list (list 2 1) (list 3 1) (list 3 2)))
  (test (prime-sum-pairs 6)
        (list (list 2 1 3) (list 3 2 5) (list 4 1 5) (list 4 3 7) (list 5 2 7)
              (list 6 1 7) (list 6 5 11))))

(define (unique-triple n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 2 (- i 1))))
    (enumerate-interval 3 n)))

(define (sum-to-s n s)
  (define (sum l) (accumulate + 0 l))
  (filter (lambda (x) (= (sum x) s)) (unique-triple n)))

(define (ex2-41)
  (test (unique-triple 5) (list (list 3 2 1) (list 4 2 1) (list 4 3 1)
                                (list 4 3 2) (list 5 2 1) (list 5 3 1)
                                (list 5 3 2) (list 5 4 1) (list 5 4 2)
                                (list 5 4 3)))
  (test (sum-to-s 5 8) (list (list 4 3 1) (list 5 2 1))))

(define (assert v)
  (if (not v) (exit 1)))

(define (any pred l)
  (cond ((null? l) #f)
        ((pred (car l)) #t)
        (else (any pred (cdr l)))))

(define (queens board-size)
  (define empty-board nil)

  (define (collides? a b)
    (let ((a-row (car a))
          (a-col (car (cdr a)))
          (b-row (car b))
          (b-col (car (cdr b))))
      (or (= a-row b-row)
          (= a-col b-col) ; should be impossible
          (= (abs (/ (- a-row b-row) (- a-col b-col))) 1))))

  (define (safe? col positions)
    (let ((r (car (filter (lambda (x) (= col (car (cdr x)))) positions)))
          (not-r (filter (lambda (x) (not (= col (car (cdr x))))) positions)))
      (not (any (lambda (p) (collides? r p)) not-r))))

  (define (adjoin-position row col rest)
    (cons (list row col) rest))

  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

(define (ex2-42)
  (test (length (queens 8)) 92))

(define (ex2-53)
  (test (list 'a 'b 'c) '(a b c))
  (test (list (list 'george)) '((george)))
  (test (cdr '((x1 x2) (y1 y2))) '((y1 y2)))
  (test (cadr '((x1 x2) (y1 y2))) '(y1 y2))
  (test (pair? (car '(a short list))) #f)
  (test (memq 'red '((red shoes) (blue socks))) #f)
  (test (memq 'red '(red shoes blue socks)) '(red shoes blue socks)))

;; ex2-43
;; by performing the recursive call in the enumerate interval loop it is
;; repeated board-size times each recursion. This increases the time from
;; T to (board-size ^ board-size)T

(define (ex2-54)
  (define (equal? a b)
    (if (and (pair? a) (pair? b))
      (if (equal? (car a) (car b))
        (equal? (cdr a) (cdr b))
        #f)
      (eq? a b)))

  (test (equal? '(a b c) '(a b c)) #t)
  (test (equal? '(a b c d) '(a (b c) d)) #f))

(define (ex2-55)
  ;; this occurs because ''abracadabra is expanded to '(quote abracadabra),
  ;; the first element of which is the quote function
  (test (car ''abracadabra) 'quote)
  (test (cdr ''abracadabra) '(abracadabra)))

(define (all pred? l)
  (cond ((null? l) #t)
        ((not (pred? (car l))) #f)
        (else (all pred? (cdr l)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . args)
  (let ((pargs (filter (lambda (x) (not (=number? x 0))) args)))
    (cond ((all number? pargs) (accumulate + 0 pargs))
          ((= (length pargs) 0) 0)
          ((= (length pargs) 1) (car pargs))
          (else (cons '+ pargs)))))

(define (make-product . args)
  (let ((pargs (filter (lambda (x) (not (=number? x 1))) args)))
   (cond ((any (lambda (x) (=number? x 0)) pargs) 0)
         ((all number? pargs) (accumulate * 1 pargs))
         ((= (length pargs) 0) 1)
         ((= (length pargs) 1) (car pargs))
         (else (cons '* pargs)))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (= 3 (length s))
    (caddr s)
    (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (= 3 (length p))
    (caddr p)
    (cons '* (cddr p))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp) (- (exponent exp) 1))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (ex2-56)
  (test (deriv '(+ x 3) 'x) 1)
  (test (deriv '(* x y) 'x) 'y)
  (test (deriv '(* (* x y) (+ x 3)) 'x)
        '(+ (* x y) (* y (+ x 3))))
  (test (deriv '(** x 3) 'x) '(* 3 (** x 2)))
  (test (deriv '(** x 2) 'x) '(* 2 x)))

(define (ex2-57)
  (test (deriv '(+ 3 x y) 'x) 1)
  (test (deriv '(* 3 x y) 'x) '(* 3 y))
  (test (deriv '(* x y (+ x 3)) 'x)
        '(+ (* x y) (* y (+ x 3)))))

(define (nmemq v l)
  (cond ((or (null? l) (null? (cdr l))) #f)
        ((eq? v (cadr l)) l)
        (else (nmemq v (cdr l)))))

(define (ex2-58)
  (define (sum? exp)
    (and
      (not (product? exp))
      (not (exponentiation? exp))
      (any (lambda (e) (eq? e '+)) exp)))

  (define (product? exp)
    (and
      (not (exponentiation? exp))
      (any (lambda (e) (eq? e '*)) exp)))

  (define (exponentiation? exp)
    (any (lambda (e) (eq? e '**)) exp))

  (define (addend exp)
    (car (nmemq '+ exp)))

  (define (augend exp)
    (let ((a (cdr (memq '+ exp))))
     (if (= (length a) 1)
       (car a)
       a)))

  (define (multiplier exp)
    (car (nmemq '* exp)))

  (define (multiplicand exp)
    (let ((m (cdr (memq '* exp))))
      (if (= (length m) 1)
        (car m)
        m)))

  (define (base exp)
    (car (nmemq '** exp)))

  (define (exponent exp)
    (let ((e (cdr (memq '** exp))))
     (if (= (length e) 1)
       (car e)
       (error "exp not fully implemented"))))

  (define (make-sum left right)
    (cond ((=number? left 0) right)
          ((=number? right 0) left)
          ((and (number? left) (number? right)) (+ left right))
          (else (list left '+ right))))

  (define (make-product left right)
    (cond ((or (=number? left 0) (=number? right 0)) 0)
          ((=number? left 1) right)
          ((=number? right 1) left)
          ((and (number? left) (number? right)) (* left right))
          (else (list left '* right))))

  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list base '** exponent))))

  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
             (make-product (multiplier exp)
                           (deriv (multiplicand exp) var))
             (make-product (deriv (multiplier exp) var)
                           (multiplicand exp))))
          ((exponentiation? exp)
           (make-product (exponent exp)
                         (make-exponentiation (base exp) (- (exponent exp) 1))))
          (else
            (error "unknown expression type -- DERIV" exp))))

  (test (deriv '(x + 3) 'x) 1)
  (test (deriv '(x * y) 'x) 'y)
  (test (deriv '(x * y * (x + 3)) 'x)
        '((x * y) + (y * (x + 3))))
  (test (deriv '(x ** 3) 'x) '(3 * (x ** 2)))
  (test (deriv '(x ** 2) 'x) '(2 * x))
  (test (deriv '(3 + x + y) 'x) 1)
  (test (deriv '(3 * x * y) 'x) '(3 * y))
  (test (deriv '(x * y * (x + 3)) 'x)
        '((x * y) + (y * (x + 3)))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define (ex2-59)
  (test (union-set '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
  (test (union-set '(1 2 3) '(1 2 3)) '(1 2 3))
  (test (union-set '() '(1 2 3)) '(1 2 3))
  (test (union-set '(1 2 3) '()) '(1 2 3)))

(define element-of-dset? element-of-set?)

(define adjoin-dset cons)

(define intersection-dset intersection-set)

(define union-dset append)

(define (ex2-60)
  ;; this representation is much slower at lookups but very fast at insertion,
  ;; so in the case of an insertion heavy workload with minimal lookups it could
  ;; be used (but probably still a bad idea)

  (test (element-of-dset? 1 '(2 2 1 3 4)) #t)
  (test (element-of-dset? 2 '(1 2 2 3 4)) #t)
  (test (element-of-dset? 5 '(1 2 2 3 4)) #f)

  (test (adjoin-dset 1 '(2 3 4)) '(1 2 3 4))
  (test (adjoin-dset 2 '(2 3 4)) '(2 2 3 4))

  (test (intersection-dset '(2 2 3 4) '(3 3 2 4)) '(2 2 3 4))
  (test (intersection-dset '(2 2 5 3) '(3 3 2 4)) '(2 2 3))
  (test (intersection-dset '(1 2 3) '(4 5 6)) '())

  (test (union-dset '(1 2 2) '(2 2 3 4)) '(1 2 2 2 2 3 4)))

(define (element-of-oset? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-oset? x (cdr set)))))

(define (intersection-oset set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
     (cond ((= x1 x2)
            (cons x1 (intersection-oset (cdr set1) (cdr set2))))
           ((< x1 x2) (intersection-oset (cdr set1) set2))
           ((< x2 x1) (intersection-oset set1 (cdr set2)))))))

(define (adjoin-oset x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set) (adjoin-oset x (cdr set))))))

(define (ex2-61)
  (test (adjoin-oset 1 '()) '(1))
  (test (adjoin-oset 3 '(1 2)) '(1 2 3))
  (test (adjoin-oset 2 '(1 3)) '(1 2 3))
  (test (adjoin-oset 1 '(2 3)) '(1 2 3)))

(define (union-oset set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-oset (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-oset (cdr set1) set2)))
                  ((< x2 x1) (cons x2 (union-oset set1 (cdr set2)))))))))

(define (ex2-62)
  (test (union-oset '(1) '()) '(1))
  (test (union-oset '() '(1)) '(1))
  (test (union-oset '(2) '(1)) '(1 2))
  (test (union-oset '(1) '(2)) '(1 2))
  (test (union-oset '(1 3) '(2 4)) '(1 2 3 4)))


(define (entry tree) (car tree))
(define (tree-left-branch tree) (cadr tree))
(define (tree-right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tset? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-tset? x (tree-left-branch set)))
        ((> x (entry set))
         (element-of-tset? x (tree-right-branch set)))))

(define (adjoin-tset x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tset x (tree-left-branch set))
                    (tree-right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (tree-left-branch set)
                    (adjoin-tset x (tree-right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (tree-left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (tree-right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (tree-left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (tree-right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (ex2-63)
  (define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
  (define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
  (define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

  ;; tree->list-1 in O(n log n)
  ;; tree->list-2 in O(n)
  ;; they both produce the same list
  (test (equal? (tree->list-1 t1) (tree->list-2 t1)) #t)
  (test (equal? (tree->list-1 t2) (tree->list-2 t2)) #t)
  (test (equal? (tree->list-1 t3) (tree->list-2 t3)) #t))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
     (let ((left-result (partial-tree elts left-size)))
      (let ((left-tree (car left-result))
            (non-left-elts (cdr left-result))
            (right-size (- n (+ left-size 1))))
        (let ((this-entry (car non-left-elts))
              (right-result (partial-tree (cdr non-left-elts)
                                          right-size)))
          (let ((right-tree (car right-result))
                (remaining-elts (cdr right-result)))
            (cons (make-tree this-entry left-tree right-tree)
                  remaining-elts))))))))

(define (flip f)
  (lambda (a b) (f b a)))

(define (ex2-64)
  ;; a. the list is split into the median, and the elements smaller and larger
  ;;    than it, from which the subtrees are built
  ;; b. we have 2 constant time splits, and we operate on each element in the
  ;;    list for a time of O(n)
  (test (list->tree '(1 3 5 7 9 11))
        '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))))

(define (ex2-65)
  (define (union-set a b)
    (fold-left (flip adjoin-tset) a (tree->list-2 b)))

  (define (intersection-set a b)
    (list->tree (filter (lambda (x) (element-of-tset? x a))
                        (tree->list-2 b))))

  (test (union-set '(2 (1 () ()) (3 () ())) '(1 () (2 () (3 () ()))))
        '(2 (1 () ()) (3 () ())))

  (test (intersection-set (list->tree '(1 3 5 7 9 11))
                          (list->tree '(1 2 3 4 5)))
        '(3 (1 () ()) (5 () ()))))

(define key car)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (cadr set-of-records)))
        (else (lookup given-key (caddr set-of-records)))))

(define (ex2-66)
  (test (lookup 1 '((3 b) ((1 a) () ()) ((5 c) () ()))) '(1 a))
  (test (lookup 2 '((3 b) ((1 a) () ()) ((5 c) () ()))) #f))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (ct-left-branch tree) (car tree))

(define (ct-right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
       (if (leaf? next-branch)
         (cons (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
         (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (ct-left-branch branch))
        ((= bit 1) (ct-right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-wset x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-wset x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
     (adjoin-wset (make-leaf (car pair)      ; symbol
                             (cadr pair))   ; frequency
                  (make-leaf-set (cdr pairs))))))

(define (ex2-67)
  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

  (test (decode sample-message sample-tree)
        '(A D A B B C A)))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (ct-left-branch tree)))
         (cons 0 (encode-symbol symbol (ct-left-branch tree))))
        ((memq symbol (symbols (ct-right-branch tree)))
         (cons 1 (encode-symbol symbol (ct-right-branch tree))))
        (else (error "unhandled symbol -- ENCODE-SYMBOL" symbol))))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (ex2-68)
  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

  (test (encode '(A D A B B C A) sample-tree) sample-message))

(define (successive-merge leaf-set)
  (if (null? leaf-set)
    #f
    (fold-left (flip make-code-tree) (car leaf-set) (cdr leaf-set)))
  )

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (ex2-69)
  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))

  (define pairs '((A 4) (B 2) (D 1) (C 1)))
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

  (test (generate-huffman-tree pairs) sample-tree)
  (test (encode '(A D A B B C A) (generate-huffman-tree pairs)) sample-message))

(for-each run-test '(ex2-1 ex2-2 ex2-3 ex2-4 ex2-5 ex2-6 ex2-7 ex2-8 ex2-9
                           ex2-10))
(print "\nex2-11 has no tests\n")
(for-each run-test '(ex2-12 ex2-13 ex2-14))
(newline)
(for-each print '("ex2-15 omitted\n"
                  "ex2-16 omitted\n"))
(for-each run-test '(ex2-17 ex2-18 ex2-19 ex2-20 ex2-21))
(print "\nex2-22 has no tests\n")
(for-each run-test '(ex2-23 ex2-24 ex2-25 ex2-26 ex2-27 ex2-28 ex2-29 ex2-30
                            ex2-31 ex2-32 ex2-33 ex2-34 ex2-35 ex2-36 ex2-37
                            ex2-38 ex2-39 ex2-40 ex2-41 ex2-42))
(print "\nex2-43 has no tests\n")
(print "picture language questions omitted for now\n")
(for-each run-test '(ex2-53 ex2-54 ex2-55 ex2-56 ex2-57 ex2-58 ex2-59 ex2-60
                            ex2-61 ex2-62 ex2-63 ex2-64 ex2-65 ex2-66 ex2-67
                            ex2-68 ex2-69))

(exit fail-count)
