#lang sketching

(define (setup)
  (size 640 360))

(define (draw)
  (fill (random 256))
  (rect (random width) (random height) 100 100))
