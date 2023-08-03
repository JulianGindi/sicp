#lang sicp
(#%require "utils.rkt")

; my own take on the infix notation differentiation exercise
; I was just dragging with that one and wanted to find a slightly simpler
; exercise to work on.
; In this case, we will just make a simple infix notation sum function.
; (isum '(2 + 4 + 5 + 6)) => 17

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s)
  (if (> (length (cddr s)) 1)
      (cddr s)
      (caddr s)))