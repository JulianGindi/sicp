#lang sicp

(define (square n)
  (* n n))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* x higher-terms) this-coeff))
   0
   coefficient-sequence))

(define (enumerate-tree tree) 
   (cond ((null? tree) nil) 
         ((not (pair? tree)) (list tree)) 
         (else (append (enumerate-tree (car tree)) 
                       (enumerate-tree (cdr tree)))))) 

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define a '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

