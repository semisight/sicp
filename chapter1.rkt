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

;In this case, scheme evaluates (p) before applying `test`, and `p` never
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

;def 1: (+ 4 3)
;(inc (+ 3 3))
;(inc (inc (+ 2 3)))
;(inc (inc (inc (+ 1 3))))
;this process is linearly recursive

;def 2: (+ 4 3)
;(+ 2 4)
;(+ 1 5)
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

(define (f-iter f-inc f f-dec n)
  (if (< n 3)
      f
      (f-iter
       (+ (f-inc) (* 2 f) (* 3 f-dec)) f-inc f (dec n))))

(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 3))))