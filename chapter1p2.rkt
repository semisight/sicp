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

;these tests are currently unreliable because of the backup running on my
;macbook
