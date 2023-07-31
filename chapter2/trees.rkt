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




