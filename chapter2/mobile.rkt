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

(define (torque b)
  (* (branch-length b) (total-weight b)))

(define (balanced? m)
  (if (not (pair? m))
      #t
      (and (= (torque (branch-structure (left-branch m))) (torque (branch-structure (right-branch m))))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))))
          


; some example branches and mobiles
; ---------------------------------
(define m1 (make-mobile (make-branch 2 3) (make-branch 2 3))) 
(define m2 (make-mobile 
            (make-branch 4 6) 
            (make-branch 5 
                         (make-mobile 
                          (make-branch 3 7) 
                          (make-branch 9 8))))) 

