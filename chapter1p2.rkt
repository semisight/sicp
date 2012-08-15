#lang planet neil/sicp

;ex 1.23

(define (square a)
  (* a a))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next a)
    (if (= a 2)
        3
        (+ a 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;commented out for exercise 1.24
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes a b)
  (cond ((> a b))
        ((even? a) (timed-prime-test (inc a)) (search-for-primes (+ 3 a) b))
        ((odd? a) (timed-prime-test a) (search-for-primes (+ 2 a) b))))

;smallest > 1,000,000 avg time is: 133.33. This is less than the double speedup
;promised in theory, but it can be explained by the use of a more complex
;algorithm.

;ex 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (dec exp) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (inc (random (dec n)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (dec times)))
        (else #f)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

;I expect the tests of numbers to take ~2 times as long. Tests ~1000 average
;66, while tests ~1,000,000 average 101.33. This will give us a ratio of ~1.5.
;We can explain the discrepancy in the ratio with overhead costs. In this case,
;the numbers tested are small, but the fermat-test will be run 10 times
;regardless of the size of the number. The calling overhead can become
;significant for small numbers.

;ex 1.25

;Mathematically, it is the same. Computationally, the larger numbers get, the
;more expensive it is to compute with them. The (exponentially) large number
;that we are dividing gets more and more expensive to compute as it grows
;larger. By contrast, the `expmod` that we wrote performs a `remainder`
;operation at each step, keeping the exponential from getting too large.

;ex 1.26

;One of the basic characteristics of a log(n) process is that it halves (or at
;least divides the problem space into a certain number of parts) on each step.
;In this case, Louis has created an unfortunate bug where, due to scheme's
;applicative-order nature, evaluates the same `expmod` twice, creating an
;inefficient tree recursion. This also *doubles* the problem space on each step
;(because the function does everything twice), eliminating the log(n) advantage
;and making it linear time.

;ex 1.27

(define (full-prime? n)
  (define (iter n a)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (iter n (inc a)))
          (else #f)))
  (iter n 1))

;As expected, the Carmichael numbers all pass the `full-prime?` test.

;ex 1.28

(define (not= a b)
  (not (= a b)))

(define (square-mr a n)
  (if (and (and (not= a 1) (not= a (dec n)))
           (= (remainder (square a) n) 1))
      0
      (remainder (square a) n)))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (square-mr (expmod-mr base (/ exp 2) m) m))
        (else
          (remainder (* base (expmod-mr base (dec exp) m)) m))))

(define (mr-prime? n)
  (define (try-it a)
     (= (expmod-mr a (dec n) n) 1))
  (try-it (+ 2 (random (- n 2)))))

;this was a very confusing problem--not because of the problem itself but the
;it was written (very ambiguous).

;ex 1.29

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons f a b n)
  (define (y k)
    (f (+ a (* k (/ (- b a) n)))))
  (define (iter k acc)
    (cond ((> k n) (/ (* acc (/ (- b a) n)) 3))
          ((or (= k 0) (= k n)) (iter (inc k) (+ acc (y k))))
          ((odd? k) (iter (inc k) (+ acc (* 4 (y k)))))
          ((even? k) (iter (inc k) (+ acc (* 2 (y k)))))))
  (iter 0 0))

;for some reason, this is surprisingly accurate even with low n (i.e. 4).
;Somehow, Racket guesses or knows the true value of the integral. For instance,
;`(simpsons cube 0 1 4)` returns the *fraction* 1/4. Meanwhile, `integral` only
;returns approximations.

;ex 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;ex 1.31

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iter values 1 inc n))

;this uses `values`, the identity primitive in scheme.

(define (pi-term n)
  (/ (* (dec n) (inc n)) (square n))) 

(define (pi-next n)
  (+ 2 n))

(define (pi-approx accuracy)
  (product-iter pi-term 3 pi-next (+ 3 (* 2 accuracy))))

;with `(* 4 (pi-approx 1000))` we get ~3.125

;and part b

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

;ex 1.32

(define (accum comb null-val term a next b)
  (if (> a b)
      null-val
      (comb (term a)
         (accum comb null-val term (next a) next b))))

(define (prd-acc t a n b)
  (accum * 1 t a n b))

;and part b

(define (accum-iter comb null-val term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (comb result (term a)))))
  (iter a null-val))

(define (sum-acc t a n b)
  (accum-iter + 0 t a n b))

;ex 1.33

(define (filter-accum pred? comb null-val term a next b)
  (if (> a b)
      null-val
      (if (pred? a)
          (comb (term a) (filter-accum pred? comb null-val term (next a) next b))
          (filter-accum pred? comb null-val term (next a) next b))))

(define (square-primes a b)
  (filter-accum full-prime? + 0 square a inc b))

;ex 1.34

;to find out what happens, let's trace the interpreter's calls:

;    (f f)
;    (f 2)
;    (2 2)

;however, `2` is not a function! The interpreter will not be able to execute,
;and will throw an error up to the user.

;ex 1.35

;the definition of the golden ratio is the number that satisfies the equation
;phi^2 = phi + 1. If we divide by phi, we get phi = 1 + 1/phi, and if we
;substitute phi for the unknown 'x' (as we want to calculate phi) we get the
;desired transformation x -> 1 + 1/x.

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

;running `fixed-point` with the arguments `(lambda (x) (inc (/ 1 x)))` and 1.0 I
;get 1.61803... (with the default tolerance of .00001).

;ex 1.36

(define (fp-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-to-x-trans x)
  (/ (log 1000) (log x)))

(define (avg a b)
  (/ (+ a b) 2))

(define (x-to-x-damp x)
  (avg x (/ (log 1000) (log x))))

;it takes 38 steps without, and 14 with average damping.

;ex 1.37

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (inc i))))))
  (rec 1))

;k must be at least 11 to get 4 decimal places.

;and part b

(define (cf-iter n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (dec i) (/ (n i) (+ (d i) acc)))))
  (iter (dec k) (/ (n k) (d k))))

;ex 1.38

(define (e-approx k)
  (define (n i)
    1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (ceiling (/ i 3)))
        1.0))
  (+ 2 (cf-iter n d k)))

;ex 1.39

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (square x))))
  (define (d i)
    (dec (* 2 i)))
  (cf-iter n d k))

;ex 1.40
