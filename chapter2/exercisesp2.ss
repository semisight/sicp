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
  (if (not (pair? m))
      m
      (let ((ls (branch-structure (left-branch m)))
            (rs (branch-structure (right-branch m))))
        (+
         (if (pair? ls)
             (total-weight ls)
             ls)
         (if (pair? rs)
             (total-weight rs)
             rs)))))

(define (branch-torque b)
  (if (not (pair? b))
      0
      (* (total-weight (branch-structure b)) (branch-length b))))

(define (balanced? m)
  (if (not (pair? m))
      #t
      (let ((lb (left-branch m))
            (rb (right-branch m)))
        (and
         (= (branch-torque lb) (branch-torque rb))
         (balanced? (branch-structure lb))
         (balanced? (branch-structure rb))))))

;if I change the constructors, all I should have to change to make things work
;is the selectors.

;ex 2.30

;very similar to the scale-tree function.

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;ex 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;ex 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;ex 2.33

;given

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;solution

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

;ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;got this one slightly wrong, reversed the addition and multiplication. Corrected by
;bill the lizard (again).

;ex 2.35

;given

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;solution

(define (acc-count-leaves t)
  (accumulate (lambda (x a) (+ a (length x))) 0 (map fringe t)))

;I like bill the lizard's solution better (it's cleaner), but this does work.

;ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;ex 2.37

;given

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m)) 

(define (mat-mat m1 m2)
  (map
   (lambda (m1row)
     (map (lambda (m2col) (dot-product m1row m2col)) (trans m2)))
     m1))

(define (trans m)
  (if (null? (car m))
      nil
      (cons (map car m)
            (trans (map cdr m)))))

;testing

(define m1
  (list
   (list 1 0 0)
   (list 0 1 0)
   (list 0 1 1)))

(define m2
  (list
   (list 2 0 0)
   (list 0 2 0)
   (list 0 2 2)))
  
(define v
  (list 1 0 0 2))

(display (mat-mat m1 m2))
