#lang sicp

; definitions of different coin types

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

(define (no-more? values)
  (null? values))

(define (except-first-denomination values)
  (cdr values))

(define (first-denomination values)
  (car values))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))


(cc 100 us-coins)
; should output 292
(cc 10 us-coins)