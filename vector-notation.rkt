#lang sketching

(define n 60)
(define mx (make-vector n 0.0))
(define my (make-vector n 0.0))

(define (setup)
  (size 640 360)
  (frame-rate 60))

(define (draw)
  (background 51)
  (no-stroke)
  (fill 255 153)

  (define which (modulo frame-count n))
  (:= mx which mouse-x)
  (:= my which mouse-y)

  (for ([i n])
    (define index (modulo (+ which 1 i) n))
    (ellipse mx[index] my[index] i i)))
