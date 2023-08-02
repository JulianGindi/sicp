#lang sicp

(#%require "utils.rkt")

(define (safe? test-column positions)
  (if (safe-positions? (car positions))
      #t
      #t))

(define (safe-positions? pos)
  ; first grab the first position and compare against all others
  (let ((first (car pos)))
    (display first)))

(define (adjoin-position row col rest)
  (cons (list row col) rest))

; I'm going to do this initially only for Queens behaving as Rooks

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;(queens 2)
(define test-queens '(((1 2) (1 1)) ((2 2) (1 1)) ((1 2) (2 1)) ((2 2) (2 1))))