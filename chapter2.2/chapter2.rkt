#lang sicp

; helper functions
(define (pow num exp)
  (cond ((= exp 0) 1)
        ((= exp 1) num)
        (else (* num (pow num (- exp 1))))))


(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment startp endp)
  (cons startp endp))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (define a (/ 2.0 (+ (x-point (start-segment s)) (x-point (end-segment s)))))
  (define b (/ 2.0 (+ (y-point (start-segment s)) (y-point (end-segment s)))))
  (make-point a b))


; orig is the point representing the top left corner of the rectangle
(define (make-rect orig width height)
  (cons orig (list width height)))

(define (rect-orig rect)
  (car rect))

(define (rect-width rect)
  (car (cdr rect)))

(define (rect-height rect)
  (car (cdr (cdr rect))))

(define (area rect)
  (* (rect-width rect) (rect-height rect)))

(define (perim rect)
  (+ (* 2 (rect-width rect)) (* 2 (rect-height rect))))


; computer euclidian distance between two points
(define (dist p1 p2)
  (sqrt (+ (pow (- (x-point p2) (x-point p1)) 2)
           (pow (- (y-point p2) (y-point p1)) 2))))
  
  

(define s (make-point 4 4))
(define rect1 (make-rect s 2 2))
;(area rect1)
;(perim rect1)




