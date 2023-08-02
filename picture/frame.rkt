#lang sicp
(#%require sicp-pict)

(define (frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-origin frame)
     (vector-add 
      (vector-scale (vector-xcor v)
                  (frame-edge1 frame))
      (vector-scale (vector-ycor v)
                  (frame-edge2 frame))))))




(define s1 (make-segment (make-vect 1 1) (make-vect 2 2)))





