#lang sicp

(define zero (lambda (f) (lambda (x) x)))

; zero takes an argument f and returns a function that takes an argument "x" 
; and returns it

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(define (a1 n)
  (+ 1 n))
