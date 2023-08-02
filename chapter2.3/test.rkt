#lang sicp


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? l1 l2)
  (cond ((eq? l1 '()) #t)
        ((pair? (car l1)) (eq? (car l1) (car l2)))
        (else
         (and (eq? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))))



(equal? '(this (is a) list)
        '(this (is a) list))


(equal? '(this is a list)
        '(this is a list))