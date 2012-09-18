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

(define upper-bound cdr)

(define lower-bound car)

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
  (if (and (> (upper-bound y) 0) (< (lower-bound y) 0))
      (make-interval 0 0)
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;ex 2.11

(define (mul-interval-2 x y)
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (cond
      ((and (>= xl 0) (>= xh 0) (>= yl 0) (>= yh 0))
       (make-interval (* xl yl) (* xh yh)))
      ((and (>= xl 0) (>= xh 0) (<= yl 0) (>= yh 0))
       (make-interval (* xh yl) (* xh yh)))
      ((and (>= xl 0) (>= xh 0) (<= yl 0) (<= yh 0))
       (make-interval (* xh yl) (* xl yh)))
      
      ((and (<= xl 0) (>= xh 0) (>= yl 0) (>= yh 0))
       (make-interval (* xl yh) (* xh yh)))
      ((and (<= xl 0) (>= xh 0) (<= yl 0) (>= yh 0)) ;difficult case
       (make-interval (min (* xl yh) (* xh yl)) (max(* xl yl) (* xh yh))))
      ((and (<= xl 0) (>= xh 0) (<= yl 0) (<= yh 0))
       (make-interval (* xh yl) (* xl yl)))

      ((and (<= xl 0) (<= xh 0) (>= yl 0) (>= yh 0))
       (make-interval (* xl yh) (* xh yl)))
      ((and (<= xl 0) (<= xh 0) (<= yl 0) (>= yh 0))
       (make-interval (* xl yh) (* xl yl)))
      ((and (<= xl 0) (<= xh 0) (<= yl 0) (<= yh 0))
       (make-interval (* xh yh) (* xl yl))))))

;ex 2.12

;as given

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;problem:

(define (make-center-percent c p)
  (let ((width (* c p)))
    (make-interval (- c width) (+ c width))))

(define (percent i)
  (- (/ (upper-bound i) (center i)) 1))

;ex 2.13

;let a = c +- i be the first interval, and b = d +- j be the second interval.
;Then, the error of a * b (assuming positive error only):

;    a * b = (c + i)(d + j)
;          = cd + cj + di + ij

;but if i and j are very small, then ij is about 0. Also, we're only looking at
;error, so we don't care about the cd term.

;    error(a * b) ~ cj + di.

;ex 2.14

;as given

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;solution:

(define (test p)
  (let ((a (make-center-percent 3 p))
        (b (make-center-percent 3 p)))
    (let ((q (par1 a b))
          (r (par2 a b)))
      (display q)(newline)
      (display r)(newline)
      (display
       (make-interval
        (- (lower-bound q) (lower-bound r))
        (- (upper-bound q) (upper-bound r)))))))

;using the test procedure above, I've found that as p -> 0, the error between
;par1 and par2 also goes to 0. The error for both upper and lower bounds seems
;to be proportional to the percent width.

;ex 2.15

;unfortunately, I saw the answer to this one accidentally while I was checking
;my work for the above problem. I'll at least put it in my own words.

;basically, the par2 form of the equation is algebraically equivalent, but
;because we're using intervals, *each* division increases error. That's why the
;par2 result is wider than the par1 result.

;ex 2.16

;apparently, this is called the 'dependency' problem, and it is very hard, so
;no, I think I'll just move onto the next problem :P