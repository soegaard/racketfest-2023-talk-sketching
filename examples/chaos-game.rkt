#lang sketching
;;; https://github.com/eXodiquas/CreativeRacketCompetition2022/tree/main/Chaosgame

(require racket/list
         racket/match
         racket/math)

; small data structure to hold a point with x and y value.
(struct point (x y)) 

; draws a point to the screen, where the x and y coordinate
; specify the red and green color values of the point.
(define (point-draw p)
  (match-let ([(point x y) p])
    (fill (modulo (exact-floor x) 255) (modulo (exact-floor y) 255) 255 255)
    (circle x y 1)))

; draws a white point to the screen.
(define (fix-point-draw p)
  (match-let ([(point x y) p])
    (fill 255)
    (circle x y 5)))

; returns the point exactly between two given points.
(define (point-midpoint p1 p2)
  (match-let ([(point x1 y1) p1]
              [(point x2 y2) p2])
    (point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

; chooses two random distinct points from a collection of points.
(define (choose-two-points points)
  (define index-1 (exact-floor (random (length points))))
  (define index-2 (exact-floor (random (length points))))
  (if (= index-1 index-2)
    (choose-two-points points)
    (values (list-ref points index-1)
            (list-ref points index-2))))

(define fixed-points   #f) ; all the fixed points
(define points-to-draw #f) ; all the points that should be drawn
(define current-point  #f) ; the current point
(define paused         #t) ; flag for the paused state

(define (setup)
  (size 400 400)
  (background 0)
  (frame-rate 60)
  (no-stroke)
  (:= fixed-points '())
  (:= points-to-draw '()))

(define (draw)
  ; press any key while having at least 2 fixed points on the screen to unpause.
  (when (and key-pressed
             (> (length fixed-points) 1))
    (:= current-point (let-values ([(p1 p2) (choose-two-points fixed-points)])
                        (point-midpoint p1 p2)))
    (:= paused #f))

  ; add a fixed point at the mouse position to the screen.
  (when (mouse-pressed)
    (:= fixed-points (cons (point mouse-x mouse-y) fixed-points)))

  ; draw all the fixed points
  (for ([p fixed-points])
    (fix-point-draw p))

  (unless paused
    ; do all the generating stuff 500 times per frame to speed up the animation.
    (for ([i (range 500)])
      ; get point between current-point and a random fixed point
      ; and draw it on the screen.
      (:= current-point (let-values ([(p1 p2) (choose-two-points fixed-points)])
                          (point-midpoint current-point p2)))
      (point-draw current-point))))
