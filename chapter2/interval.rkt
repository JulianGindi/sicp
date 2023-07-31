#lang sicp

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (lower-bound y))
                 (- (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (if (or (and (< 0 (lower-bound x)) (> 0 (upper-bound x)))
          (and (> 0 (lower-bound x)) (< 0 (upper-bound x)))
          (and (< 0 (lower-bound y)) (> 0 (upper-bound y)))
          (and (> 0 (lower-bound y)) (< 0 (upper-bound y))))
      (error "One or more interval crossing zero.")
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define (iwidth i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))



; testing exercise 2.9. Width as a function of inputs.

(define ia (make-interval 2 5)) ; width of 1.5
(define ib (make-interval 5 6)) ; width of 0.5
; width of sum
;(iwidth (add-interval ia ib)) ; width of 2.0

; another set of intervals
(define ic (make-interval 3 11)) ; width of 4.0
(define id (make-interval 1 7)) ; width of 3.0
; width of sum
;(iwidth (add-interval ic id)) ; width of 7.0

; summing is just adding the widths together, so its a function of the
; widths of the input

; width of product
;(iwidth (mul-interval ia ib)) ; width of 10.0
;(iwidth (mul-interval ic id)) ; width of 37.0
; not super correlated