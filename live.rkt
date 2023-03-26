#lang sketching

(define t 0)

(define (setup)
  (size 640 360)
  (color-mode 'hsb 100 100 100)
  )
(
 (define (draw)
   (:= t (+ t 1))
  (fill t 100 100)
  (rect (random 640) (random 360)  100 100))

