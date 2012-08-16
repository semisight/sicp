#lang planet neil/sicp

;ex 2.1

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (pos (>= (* n d) 0)))
    (let ((na (abs (/ n g)))
          (da (abs (/ d g))))
      (if pos
          (cons na da)
          (cons (- na) da)))))

;ex 2.2

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (make-point
   (average (x-point (start-segment s)) (x-point (end-segment s)))
   (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;ex 2.3

(define make-rect cons)
(define top-left-point car)
(define bot-right-point cdr)

(define (area r)
  (let ((a (top-left-point r))
        (b (bot-right-point r)))
    (abs (* (- (x-point a) (x-point b))
            (- (y-point a) (y-point b))))))

;I'm not sure what the second half of this question is really asking... if I
;use top-right and bottom-left corners it will still work. I believe what
;they're asking me to do is to write a general version that takes selectors as
;inputs and applies them to the rectangle object... if so, here it is!

(define (area-general point-1-sel point-2-sel r)
  (let ((a (point-1-sel r))
        (b (point-2-sel r)))
    (abs (* (- (x-point a) (x-point b))
            (- (y-point a) (y-point b))))))

;ex 2.4

(define (pcons x y)
  (lambda (m) (m x y)))

(define (pcar z)
  (z (lambda (p q) p)))


(define (pcdr z)
  (z (lambda (p q) q)))

;ex 2.5

(define (qcons a b)
  (define (iter n pow acc)
    (if (= pow 0)
        acc
        (iter n (dec pow) (* n acc))))
  (* (iter 2 a 1) (iter 3 b 1)))

(define (factor n fact)
  (if (= (remainder n fact) 0)
      (inc (factor (/ n fact) fact))
      0))

(define (qcar p)
  (factor p 2))

(define (qcdr p)
  (factor p 3))

;ex 2.6

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (church-add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;ex 2.7

(define make-interval cons)

(define upper-bound car)

(define lower-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;commented out for ex 2.10
;(define (div-interval x y)
;  (mul-interval x 
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))

;ex 2.8

;you can find the difference of two intervals by taking the absolute value of
;the difference of the lower and upper bounds.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;ex 2.9

;to check to see how the operation affects the width of the resulting interval
;we need to find a way to use the identity width = upper-lower to find each as a
;function of width.

;addition:
;l(x)+l(y) = l(t)
;u(x)+u(y) = u(t)

;l(x)+wx+l(y)+wy = u(t)

;wt*2 = u(t)-l(t) = l(x)-l(x)+wx + l(y)-l(y)+wy = wx+wy
;wt = wx+wy / 2

;this should hold for subtraction as well because subtraction is addition of the
;opposite.

;ex 2.10

(define (div-interval x y)
  (if (= (- (upper-bound y) (lower-bound y)) 0)
      (make-interval 0 0)
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;ex 2.11
