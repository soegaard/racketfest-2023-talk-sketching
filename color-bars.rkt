#lang sketching

;; One time setup
(define (setup)  
  (size 640 360)
  (set-frame-rate! 30)
  (color-mode 'hsb 100 100 100))


;; Initial State
(define t 0)   ; time
(define r 100) ; radius

;; Called once each frame
(define (draw)
  ;; Update state
  (:= t (+ t 1))
  ;; Render
  (background 128)
  (define x (+ (/ width  2) (* r (cos (* 2. π (/ t 120.))))))
  (define y (+ (/ height 2) (* r (sin (* 2. π (/ t 120.))))))
  (translate (/ width 2) (/ height 2))
  (rotate (radians t))
  (fill t 100 100)
  (rect x y 20 20))


;; (define (setup)
;;   (size 640 360)  
;;   (color-mode 'hsb height height height)
;;   (background 0)
;;   #;(no-stroke))


;; (define (draw)
;;   (fill mouse-y mouse-x height)
;;   (rect 10 10 620 340))


  ;; (define which-bar (quotient mouse-x bar-width))
  ;; (unless (= which-bar last-bar)
  ;;   (define bar-x (* which-bar bar-width))
  ;;   (fill mouse-y height height)
  ;;   (rect bar-x 0 bar-width height)
  ;;   (set! last-bar which-bar)))




#;(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (unless (= which-bar last-bar)
    (define bar-x (* which-bar bar-width))
    (fill mouse-y height height)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))
