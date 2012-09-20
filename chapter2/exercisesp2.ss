#lang planet neil/sicp

;ex 2.17

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;ex 2.18

(define (reverse l)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons (car a) b))))
  (iter l '()))

;ex 2.19

;given

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;solution:

(define no-more? null?)

(define except-first-denomination cdr)

(define first-denomination car)

;The order does *not* matter -- because the program finds every combination, it
;goes through all the sub-lists.

;ex 2.20

(define (same-parity h . t)
  (define (filter p? l)
    (if (null? l)
        l
        (if (p? (car l))
            (cons (car l) (filter p? (cdr l)))
            (filter p? (cdr l)))))
  (if (even? h)
      (filter even? (cons h t))
      (filter odd? (cons h t))))

;ex 2.21

(define (square x)
  (* x x))

(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

;ex 2.22

;given

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

;this is interesting, because it is almost word-for-word what my reverse
;function is for exercise 2.18. The reason it is backwards is that iter builds
;a list in answer, it takes the next item from things and puts it in *front*.
;Visually:

;    things   answer
;    (1 2 3)  ()
;    (2 3)    (1)
;    (3)      (4 1)
;    ()       (9 4 1)

;Louis' second attempt is:

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;Which builds a list that looks like (cons (cons (cons nil 1) 4 9). This is the
;right order (technically), but it is the incorrect format for a list. A proper
;list should have the form (cons 1 (cons 4 (cons 9 nil))).

;ex 2.23

(define (for-each proc items)
  (cond
    ((null? items) #t)
    (else
     (proc (car items))
     (for-each proc (cdr items)))))

;ex 2.24

;The interpreter returns (1 (2 (3 4))).

;(1 (2 (3 4)))  '(2 (3 4))
;[*][*]-------->[*][/]
; |              |
;[1]             | (2 (3 4))  '(3 4)
;               [*][*]------->[*][/]
;                |             |
;               [2]            | (3 4)
;                             [*][*]--->[*][/]
;                              |         |
;                             [3]       [4]

;   *
;  / \
; 1   *
;    / \
;   2   *
;      / \
;     3   4

;ex 2.25

;(car (cdr (car (cdr (cdr ...)))))
;(car (car ...))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ...))))))))))))

;ex 2.26

;append: (1 2 3 4 5 6)
;cons: (1 2 3 (4 5 6))
;list: ((1 2 3) (4 5 6))

;ex 2.27

;given

(define x (list (list 1 2) (list 3 4)))

;solution

(define (deep-reverse l)
  (define (iter a b)
    (if (null? a)
        b
        (iter (cdr a) (cons 
                       (if (pair? (car a))
                           (reverse (car a))
                           (car a)) b))))
  (iter l '()))

;ex 2.28

(define (fringe t)
  (cond ((null? t) nil)
        ((not (pair? t)) (list t))
        (else (append (fringe (car t))
                      (fringe (cdr t))))))

;this exercise may have been inspired in part by Bill the Lizard's own work.
;I've seen this problem before on 4Clojure, and I'd never been able to solve it.
;I guess now I know how.

;ex 2.29

;given

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

(define (total-weight m)
  (let ((ls (branch-structure (left-branch m)))
        (rs (branch-structure (right-branch m))))
    (+
     (if (pair? ls)
         (total-weight ls)
         ls)
     (if (pair? rs)
         (total-weight rs)
         rs))))

(define (balanced? m)
  (let ((ls (branch-structure (left-branch m)))
        (rs (branch-structure (right-branch m)))
        (lb (left-branch m))
        (rb (right-branch m)))
    (and
     (=
     (* (branch-length lb)
        (if (pair? ls)
            (total-weight ls)
            ls))
     (* (branch-length rb)
        (if (pair? rs)
            (total-weight rs)
            rs)))
     (if (pri
     
(define mob
  (make-mobile
   (make-branch 2 3)
   (make-branch 1 (make-mobile
                 (make-branch 5 1) (make-branch 1 2)))))