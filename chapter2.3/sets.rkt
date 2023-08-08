#lang sicp
(#%require "utils.rkt")

(define (uset l)
  (cond ((null? l) '())
        ((element-of-set? (car l) (cdr l))
         (uset (cdr l)))
        (else (cons (car l) (uset (cdr l))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1)
         set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(define set1 '(1 2 3 4 5))
(define set2 '(3 4 5 6 7 8))
(define set3 (uset '(2 3 2 1 3 2 2)))
