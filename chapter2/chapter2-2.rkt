#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(define (reverse items)
  (define (iter items results)
    (if (null? items)
        results
        (iter (cdr items) (cons (car items) results))))
  (iter items '()))

; function that filters a list to only include values that
; return #t when passed to "f"
(define (filter f items)
  (cond ((null? items) '())
        ((f (car items)) (cons (car items) (filter f (cdr items))))
        (else (filter f (cdr items)))))

; iterative version
(define (filter-iter f items)
  (define (iter f items out)
    (cond ((null? items) out)
          ((f (car items)) (iter f (cdr items) (cons (car items) out)))
          (else (iter f (cdr items) out))))
  (reverse (iter f items '())))       

(define (same-parity . vals)
  (if (odd? (car vals))
      (filter odd? vals)
      (filter even? vals)))


(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (msquare-list items)
  (map (lambda (x) (* x x)) items))


(msquare-list (list 1 2 3 4))
