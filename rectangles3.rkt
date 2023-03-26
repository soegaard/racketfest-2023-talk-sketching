#lang sketching

(define (setup)
  (size 640 360)
  (color-mode 'hsb 100 100 100))

(define (draw)
  (fill (random 100) 100 100)
  (rect mouse-x mouse-y 100 100))
