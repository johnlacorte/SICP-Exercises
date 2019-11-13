#lang sicp
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;;Ex 1.2
;;(/ (+ 5 4 (- 2 3 (+ 6 (/ 1 5))))
;;   (* 3 (- 6 3) (- 3 7))))

;;Ex 1.3
(define (sum-of-largest-squares x y z)
  (cond ( (and (< x y) (< x z)) (sum-of-squares y z) )
        ( (and (< y x) (< y z)) (sum-of-squares x z) )
        ( (and (< z x) (< z y)) (sum-of-squares x y) )
        ))

;;Ex 1.4
;;In the example the if in the procedure returns a procedure to be applied to the two arguments.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;Ex 1.5
;;p in the example would cause an error if p is resolved, this would indicate applicative resolution

;;Ex 1.6
;;Compare new-if to if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (cube x)
  (* x x x))

;;Ex 1.8
(define (cube-root x)
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve-guess guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve-guess guess))))
  (cube-root-iter 1.0))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;Ex 1.9

;;Ex 1.10

(define (fib n)
  (define (fib-iter x y)
    (if (> (+ x y) n)
        (cons x (cons y '()))
        (cons x (fib-iter y (+ x y)))))
  (fib-iter 0 1))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;Ex 1.11
;;(define (f n)
;;  (if (< n 3)
;;      n
;;      (+ (f (- n 1))
;;         (* 2 (f (- n 2)))
;;         (* 3 (f (- n 3))))))

(define (f n)
  (define (f-iter x)
    (if (= x 1)
        (- n 1)
        (+ (* x (- n x) (f-iter (- x 1))))))
  (if (< n 3)
      n
      (f-iter 3)))

;;Ex 1.12
(define (pascals-triangle row column)
  (if (or (= column 1) (= column row))
      1
      (+ (pascals-triangle (- row 1) (- column 1))
         (pascals-triangle (- row 1) column))))

;;Ex 1.13

;;Ex 1.14

;;Ex 1.15
(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (fast-expt b (- n 1)))))

;;Ex 1.16

;;Ex 1.17
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiply a b)
  (if (= b 0)
      0
      (if (even? b)
          (multiply (double a) (halve b))
          (+ a (multiply a (- b 1))))))

;;Ex 1.18
(define (multiply2 a b)
  (define (iter total a b)
    (cond ((= b 0) total)
          ((even? b) (iter total (double a) (halve b)))
          (else (iter (+ total a) a (- b 1)))))
  (iter 0 a b))

;; Ex1.19
;;(define (fast-fib n)
;;  (define (fib-iter a b p q count)
;;    (cond ((= count 0) b)
;;          ((even? count)
;;            (fib-iter a
;;                      b
;;                      <??>
;;                      <??>
;;                      (/ count 2)))
;;           (else (fib-iter (+ (* b q) (* a q) (* a p))
;;                           (+ (* b p) (* a q))
;;                           p
;;                           q
;;                           (- count 1)))))
;;  (fib-iter 1 0 0 1 n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;Ex 1.20

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
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;Ex 1.21

;;Ex 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes start end)
  (if (< start end)
      (timed-prime-test start)
      (search-for-primes (+ start 2) end))
)

;;Ex 1.23

;;Ex 1.24

;;Ex 1.25

;;Ex 1.26

;;Ex 1.27

;;Ex 1.28

;;Start part 1.3

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(define odds (list 1 3 5 7))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l))
      (list (car l)))))

(define (same-parity x . y)
  (define (same-parity-recurse x y)
    (define (parity-check)
      (or (and (odd? x) (odd? (car y)))
          (and (even? x) (even? (car y)))))
    (if (null? y)
        nil
        (if (parity-check)
            (cons (car y) (same-parity-recurse x (cdr y)))
            (same-parity-recurse x (cdr y)))))
  (cons x (same-parity-recurse x y)))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
  
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
;(map (lambda (x) (* x x))
;     (list 1 2 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;(count-leaves (list 1 (list 2 (list 3 4))))

;(define mylist (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr mylist)))))

;(define items (list (list 7)))
;(car (car items))

;(define items (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items))))))))))))
;(cadr (cadr (cadr (cadr (cadr (cadr items))))))

(define (deep-reverse l)
  (if (pair? l)
      (append (deep-reverse (cdr l))
              (list (deep-reverse (car l))))
      l))

(define (fringe tree)
  (if (null? tree)
      nil
      (if (pair? tree)
          (append (fringe (car tree))
                  (fringe (cdr tree)))
          (list tree))))
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (branch-torque branch)
  (* (branch-length branch)
     (find-weight branch)))
(define (mobile? branch)
  (if (pair? (branch-structure branch))
      #t
      #f))
(define (mobile-balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (if (mobile? (left-branch mobile))
           (mobile-balanced? (branch-structure (left-branch mobile)))
           #t)
       (if (mobile? (right-branch mobile))
           (mobile-balanced? (branch-structure (right-branch mobile)))
           #t)))
(define (total-weight mobile)
  (+ (find-weight (left-branch mobile))
     (find-weight (right-branch mobile))))
(define (find-weight branch)
    (if (mobile? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
;(define (square-tree tree)
;  (map (lambda (sub-tree)
;         (if (pair? sub-tree)
;             (square-tree sub-tree)
;             (* sub-tree sub-tree)))
;       tree))
(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))
(define (square-tree tree) (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

