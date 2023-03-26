#lang sketching

(define (setup)
  (size 640 360)
  (color-mode 'hsb 100 100 100))

(define (draw)
  (fill (random 100) 100 100)
  (rect (random width) (random height) 100 100))

