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
