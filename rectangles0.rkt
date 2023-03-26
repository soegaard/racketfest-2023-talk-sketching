#lang sketching

(define (setup)
  (size 640 360))

(define (draw)
  (rect (random width) (random height) 100 100))
