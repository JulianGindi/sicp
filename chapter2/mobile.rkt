#lang sicp


(define (make-branch length structure)
  (list length structure))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

(define (make-mobile left right)
  (list left right))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (total-weight-iter m)
  (define (iter b total)
    (if (not (pair? (branch-structure b)))
        (+ total (branch-structure b))
        (iter (branch-structure b) total)))
  (+ (iter (left-branch m) 0) (iter (right-branch m) 0)))

(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else
         (+ (total-weight (branch-structure (left-branch m)))
            (total-weight (branch-structure (right-branch m)))))))

(define (torque-branch b)
  (* (branch-length b) (total-weight b)))

; a simple function that simply measures if two
; "terminating" mobiles, that is ones who don't have other
; mobiles hanging from them.
; In a way, this is our terminating case
(define (balanced-term m)
  (= (* (branch-length (left-branch m)) (branch-structure (left-branch m)))
     (* (branch-length (right-branch m)) (branch-structure (right-branch m)))))
          


; some example branches and mobiles
; ---------------------------------
; simple two branch with weights setup
(define b1 (make-branch 6 10))
(define b2 (make-branch 3 20))
(define m1 (make-mobile b1 b2))

; a second mobile with a sub-mobile
(define b3 (make-branch 6 10))
(define b4 (make-branch 2 2))
(define b5 (make-branch 5 b4))
(define m2 (make-mobile b3 b5))
;(balanced-term m1)
