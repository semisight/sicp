#lang planet neil/sicp

;ex 1.1

;10
;12
;8
;3
;6
;N/A
;N/A
;19
;#f
;4
;16
;6
;16

;ex 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;ex 1.3

;commentary: this exercise does not specify for cases where two numbers are
;equal. I'll assume that it's not in the function domain.

(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-max-squares a b c) 
  ;we'll need two compares
  (if (> a b)
      (if (> b c)
          ;c is smallest
          (sum-of-squares a b)
          ;b is smallest
          (sum-of-squares a c))
      (if (> a c)
          ;c is smallest
          (sum-of-squares a b)
          ;a is smallest
          (sum-of-squares b c))))

;ex 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;the `if` statement in this function will return a function depending on the
;conditional. Because of this, the `if` statement appears in the statement
;surrounding it as a function/verb itself.

;ex 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
         0
         y))

;scheme will recurse over `p` indefinitely (until all the stack space is gone).
;This is because it is applicative-order; it evaluates the operator (the first
;value in the s-expression) and the operands (the other values in the s-exp)
;before applying the operator to the operands.

;In this case, scheme evaluates `(p)` before applying `test`, and `p` never
;returns.

;ex 1.6

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if pred then-cl else-cl)
  (cond (pred then-cl)
        (else else-cl)))

(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter-new (improve guess x) x)))

;when Alyssa attempts to run `sqrt-iter-new`, it will hang. `new-if` is
;applicative order, so it will evaluate *both* the then and else-clause, instead
;of only evaluating one of them. Because of this, the recursion will never end.

;ex 1.7

;`sqrt-iter` will fail on both small and large numbers. For small numbers,
;`good-enough?` is too easily satisfied. For large numbers, `good-enough?` will
;be too stingy and won't ever be satisfied.

(define (square x)
  (* x x))

(define (good-enough-2? guess x last-guess)
  ;if they are within 5% of each other.
  (< (abs (- (/ guess last-guess) 1)) 0.05))

(define (sqrt-iter-helper guess x last-guess)
  (if (good-enough-2? guess x last-guess)
      guess
      (sqrt-iter-helper (improve guess x) x guess)))

(define (sqrt-iter-2 guess x)
  (sqrt-iter-helper guess x 10000000))

(define (sqrt-2 x)
  (sqrt-iter-2 1.0 x))

;ex 1.8

;cube root approximator using the `good-enough-2?` method from ex 1.7.

(define (improve-cube x guess)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (cbrt-iter guess x last-guess)
  (if (good-enough-2? guess x last-guess)
      guess
      (cbrt-iter (improve-cube x guess) x guess)))

(define (cbrt x)
  (cbrt-iter 1.0 x 1000))

;ex 1.9

;def 1: `(+ 4 3)`
;    (inc (+ 3 3))
;    (inc (inc (+ 2 3)))
;    (inc (inc (inc (+ 1 3))))
;this process is linearly recursive

;def 2: `(+ 4 3)`
;    (+ 2 4)
;    (+ 1 5)
;this process is linearly iterative

;ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (dec x) (A x (dec y))))))

;(a 1 10): 1024
;(a 2 4): 65536
;(a 3 3): 65536

;f(n) -> A(0, n): 2*n
;g(n) -> A(1, n): g(0) = 0, g(n != 0) = 2^n
;h(n) -> A(2, n): 2^^n (where ^^ is Knuth's up arrow notation)

;ex 1.11

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (dec n)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter a b c n)
  (if (= n 0)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (dec n))))

(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 2))))

;ex 1.12

;for this exercise, we'll look at pascal's triangle as a right triangle where
;element number n (index 1-based) in r (also 1-based) is the sum of the number
;above it and the number above and to the left of it.

(define (pascal-comp n r)
  (cond
    ((< n 1) 0)
    ((> n r) 0)
    ((and (= n 1) (= r 1)) 1)
    (else (+ (pascal-comp n (dec r)) (pascal-comp (dec n) (dec r))))))

;ex 1.13

;for induction we need two things--a base case, and the proof that, given f(n),
;the equation also holds true for f(n+1). For our base case, let's first take
;fib(0).

;    fib(0) = 0 = (phi^0 - psi^0)/sqrt(5)
;    0 = (1 - 1)/sqrt(5) = 0.

;you may have to take my word for it that this holds for fib(1) and fib(2) (in
;later exercises I will have more explicit proofs). Given that these three hold,
;the equation `fib(2) = fib(1) + fib(0)` holds true, which is our full base
;case.

;with the base case proven, let's move on to the f(n) implies f(n+1) part. We
;know from the def. of fib() that fib(n+1) = fib(n) + fib(n-1). Given that
;fib(n) and fib(n-1) are equal to the above equation (with n and n-1 instead of
;0 respectively), what we have becomes:

;    fib(n+1) = (phi^n - psi^n)/sqrt(5) + (phi^(n-1) - psi^(n-1))/sqrt(5)
;    = (phi^n + phi^(n-1) - (psi^n + psi^(n-1)))/sqrt(5)
;    = (phi^(n+1)(1/phi + phi^-2) - psi^(n+1)(1/psi + psi^-2))/sqrt(5)
;    = (phi^(n+1)(1/phi)(1 + 1/phi) - psi^(n+1)(1/psi)(1 + 1/psi))/sqrt(5)

;it is here that we can take advantage of the fact that both phi and psi are
;roots of the golden ratio equation `(1 +- sqrt(5) / 2)`, and therefore follow the
;property that `1 + 1/phi = phi` (where you can also substitute psi). Moving
;forward:

;    = (phi^(n+1)(1/phi)(phi) - psi^(n+1)(1/psi)(psi))/sqrt(5)
;    = (phi^(n+1) - psi^(n+1))/sqrt(5).

;now that we have this result, we still need to show that `phi^n/sqrt(5)`
;approximates `fib(n)`. To do this, we can show that psi^n/sqrt(5) is small.
;psi is approximately -.618, which has `abs(psi) < 1`. This means that as it is
;raised to increasingly higher powers, it will tend towards 0. Ergo,
;`phi^n/sqrt(5)` ~ fib(n).

;ex 1.14

;"draw tree for 11 cents"
;    (count-change 11)
;     |
;    (cc 11 5)
;     |       \
;    (cc 11 4) (cc -39 5)
;     |       \          \
;    (cc 11 3) (cc -14 4) 0
;     |       \          \
;    (cc 11 2) (cc 1 3)   0
;     |       \        \-----------\-----------------\
;    (cc 11 1) (cc 6 2)             (cc 1 2)          (cc -9 3)
;     |       \        \\--------\          \--------\         \
;    (cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2)   (cc 1 1) (cc -4 2) 0
;     |         |         |         |         |        |
;     ...
;     0         1         1         1         1        0

;this process is similar in recursion to the recursive fibonacci solution, so
;it should likewise grow linearly in space. As for time, it behaves linearly
;when kinds-of-coins == 1. When kinds-of-coins >= 2, it seems to grow
;exponentially (edit: corrected by bill-the-lizard's solution: it grows at
;theta(n^kinds-of-coins), and because the initial kinds-of-coins == 5, it has a
;time complexity of theta(n^5)).

;ex 1.15

;the angle is cut by one third each time, so we need to solve:

;    12.15 * (1/3)^n <= 0.1
;    12.15 * 1/3^n <= 0.1
;    12.15 * 1 / 0.1 <= 3^n
;    121.5 <= 3^n
;    log(121.5)/log(3) <= n
;    4.3 <= n
;    n = 5.

;space growth is constant, since we only need to remember the most recent angle
;(then repeatedly apply `p`). Time growth is logarithmic as hinted towards by
;the presence of the log() function in the progression above. (edit: corrected
;by bill-the-lizard again: space is logarithmic as well. I should spend some
;extra time identifying space and time complexity.)

;ex 1.16

;we'll use the invariant hint that the book gives us to do this problem. ab^n
;should always be equal to the original b^n.

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        ((odd? n) (fast-expt-iter (* a b) b (dec n)))))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

;ex 1.17

(define (log-mult a b)
  (cond ((= b 1) a)
        ((even? b) (log-mult (* 2 a) (/ b 2)))
        ((odd? b) (+ a (log-mult a (dec b))))))

;ex 1.18

;I'm going to try something interesting here--what if my invariant for this
;exercise is `ab + n`, where n starts as 0, and ends as the final value?

(define (mult-iter a b n)
  (cond ((or (= a 0) (= b 0)) n)
        ((even? b) (mult-iter (* a 2) (/ b 2) n))
        ((odd? b) (mult-iter a (dec b) (+ n a)))))

(define (mult a b)
  (mult-iter a b 0))