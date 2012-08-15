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
