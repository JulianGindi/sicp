#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (matrix-*-matrix m n) 
  (let ((cols (transpose n))) 
    (map (lambda (v) (matrix-*-vector cols v)) m))) 

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define v '(2 2 2 2))

(matrix-*-vector m v)