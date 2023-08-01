#lang sicp

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x 
  (list (list 1 2) (list 3 4)))

(define (reverse items)
  (define (iter items results)
    (if (null? items)
        results
        (iter (cdr items) (cons (car items) results))))
  (iter items '()))

(define (deep-reverse items)
  (define (iter items results)
    (cond ((null? items) results)
          ((not (pair? (car items))) (iter (cdr items) (cons (car items) results)))
          (else ; we have a pair
           (iter (cdr items) (cons (iter (car items) '()) results))))) 
  (iter items '()))


(define (fringe t)
  (cond ((null? t) nil)
        ((not (pair? (car t))) (cons (car t) (fringe (cdr t))))
        (else ; we have a pair
         (append (fringe (car t)) (fringe (cdr t))))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else
         (cons (scale-tree (car tree)
                           factor)
               (scale-tree (cdr tree)
                           factor)))))

(define (square n)
  (* n n))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (msquare-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (msquare-tree sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map op tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (op tree))
        (else
         (cons (tree-map op (car tree)) (tree-map op (cdr tree))))))

(define (tmsquare-tree tree)
  (tree-map square tree))

(tmsquare-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))