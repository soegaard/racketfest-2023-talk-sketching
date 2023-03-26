#lang sketching

;; https://github.com/hasn0life/sketching-space-invaders

(define WIDTH 800)
(define HEIGHT 600)
(define SHIP-SIZE 30)
(define INVADER-SIZE 40)
(define LASER-SPEED 300)
(define LASER-SIZE 20)
(define LASER-T-LIM .5)

(define bg-color (color 25 25 112))

(struct player (x y))
(struct invader (x y points))
(struct laser (x y))
(struct explosion (x y frame))

(define my-ship '())
(define player-lasers '())
(define invader-lasers '())
(define invaders-list '())
(define explosion-list '())
(define going-down 0)
(define invader-speed 0)
(define points 0)
(define current-stage 0) 

(define (setup-invasion row col stage)
  (define c-dist (round (/ WIDTH (+ 2 col))))
  (define v-dist (* 2 INVADER-SIZE ))
  (for*/list ([r (in-range row)]
              [c (in-range col)])
    (invader (* c-dist .9 (add1 c)) (* .75 v-dist (add1 r))
             (- (* row (add1 stage) 5) (* r 5 (add1 stage))))))

(define (reset stage)
  (:= my-ship (player (/ WIDTH 2) (* HEIGHT .9)))
  (:= player-lasers '())
  (:= invader-lasers '())
  (:= explosion-list '())
  (:= invaders-list (setup-invasion 4 8 stage))
  (:= going-down 0)
  
  (cond
    [(= stage 0)
     (:= points 0)
     (:= invader-speed 15)
     (:= current-stage 0)]
    [else
     (:= invader-speed (* 15 (add1 stage)))
     (:= current-stage stage)])
  )

(define left-pressed #f)
(define right-pressed #f)
(define shoot-pressed #f)
(define pause-pressed #f)
(define paused #f)

(define prev-millis 0)
(define laser-timer LASER-T-LIM)

;; drawing functions
(define (draw-ship x y)
  (fill "gray")
  (triangle (+ x SHIP-SIZE) y
            x (- y SHIP-SIZE)
            (- x SHIP-SIZE) y )
  (fill "teal")
  (circle x (- y (/ SHIP-SIZE 2)) SHIP-SIZE )
  (fill "gray")
  (ellipse x (- y (/ SHIP-SIZE 2)) (/ SHIP-SIZE 2) (* 2 SHIP-SIZE))
  )

(define (draw-laser x y color)
  (ellipse-mode 'center)
  (fill color)
  (ellipse x y (/ LASER-SIZE 2) LASER-SIZE))

(define (draw-invader x y)
  (ellipse-mode 'center)
  (fill "gray")
  (ellipse x y INVADER-SIZE INVADER-SIZE))

(define (draw-explosion x y size)
  (fill "gray")
  (triangle (+ x (* (random 4) size)) (+ y (* (random 4) size))
            (- x (* (random 5) size)) (+ y (* (random 3) size))
            (+ x (* (random 2) size)) (- y (* (random 5) size)))
  (triangle (+ x (* (random 3) size)) (+ y (* (random 5) size))
            (- x (* (random 2) size)) (+ y (* (random 0) size))
            (+ x (* (random 4) size)) (- y (* (random 3) size)))
  (fill "orange")
  (triangle (+ x (* (random 2) size)) (+ y (* (random 2) size))
            (- x (* (random 1) size)) (+ y (* (random 4) size))
            (+ x (* (random 3) size)) (- y (* (random 0) size)))
  (triangle (+ x (* (random 3) size)) (+ y (* (random 3) size))
            (- x (* (random 3) size)) (+ y (* (random 0) size))
            (+ x (* (random 1) size)) (- y (* (random 2) size)))
  )

;; keyboard functions
(define (pressed-func is-set?)
  (when (equal? key 'right)
    (set! right-pressed is-set?))
  (when (equal? key 'left)
    (set! left-pressed is-set?))
  (when (equal? key #\space)
    (set! shoot-pressed is-set?))
  (when (equal? key #\p)
    ;; "one shot" key press means we check for state transition
    ;; whivch occurs when pause-pressed is false and is being set to true
    (when (and (not pause-pressed) is-set?)
      (:= paused (if paused #f #t)))
    (set! pause-pressed is-set?)))

(define (on-key-pressed)
  (pressed-func #t))

(define (on-key-released)
  (pressed-func #f))

;; physics functions

;; circle circle collision
(define (hit? x1 y1 x2 y2 dist)
  (<= (sqrt (+ (sqr (- x1 x2))
               (sqr (- y1 y2))))
      dist))
  
;; setup
(define (setup)
  (size WIDTH HEIGHT)
  ;(collect-garbage 'incremental)
  (background bg-color)
  (set-frame-rate! 60)
  (no-stroke)
  (reset 0)
  )


;; main function
(define (draw)
  
  (define current-millis (millis))
  (define dt (/ (- current-millis prev-millis) 1000.0))
  (set! prev-millis current-millis)
  (when paused (:= dt 0))
  (+= laser-timer dt)

  ;; controls
  (when right-pressed
    (when (< (+ my-ship.x SHIP-SIZE) WIDTH)
      (+= my-ship.x (* 100 dt))))
  (when left-pressed
    (when (> (- my-ship.x SHIP-SIZE) 0)
      (-= my-ship.x (* 100 dt))))
  (when (and shoot-pressed (not paused))
    (when (> laser-timer LASER-T-LIM)
      (:= laser-timer 0)
      (when (< (length player-lasers) 3)
        (:= player-lasers
            (cons (laser my-ship.x (- my-ship.y SHIP-SIZE))  player-lasers)))))

  ;; ai
  ;; fire laser?
  (when (not paused)
    (for ([i (in-list invaders-list)])
      (when (< (random) .02)
        ;; make sure its at the bottom
        ;; theres more efficient ways of doing this like keeping a list of
        ;; enemies at the bottom that changes as we kill them?
        (when (not (ormap (lambda (invader)
                          (and (> (add1 invader.x) i.x)
                              (< (sub1 invader.x) i.x)
                              (> invader.y i.y)))
                  invaders-list))
        (:= invader-lasers (cons (laser i.x i.y) invader-lasers)))))
    )
  
  ;; check for side wall
  (for ([i (in-list invaders-list)])
    (when (<= going-down 0)
      ;; only check if we're coming to the wall to avoid retriggereing
      (when (or (and (< i.x INVADER-SIZE) (< invader-speed 0)) 
                (and (> i.x (- WIDTH INVADER-SIZE)) (> invader-speed 0)))
        (:= going-down (/ INVADER-SIZE 2))
        (:= invader-speed (- invader-speed))
        ;; increment in the absolute value sense
        (if (< invader-speed 0)
            (-= invader-speed 5)
            (+= invader-speed 5)))))
  
    ;; physics

  (for ([i (in-list invaders-list)])
    (cond
      ([> going-down 0]
       (+= i.y (* (abs invader-speed) dt)))
      (else
       (+= i.x (* invader-speed dt)))))
  (-= going-down (* (abs invader-speed) dt))
  (when (< going-down 0) (:= going-down 0))
  
  (for ([i (in-list player-lasers)])
    (-= i.y (* LASER-SPEED dt))
    (when (< i.y 0)
      (:= player-lasers (remove i player-lasers))))

  (for ([i (in-list invader-lasers)])
    (+= i.y (* LASER-SPEED dt))
    (when (> i.y HEIGHT)
      (:= invader-lasers (remove i invader-lasers))))

  ;; player laser vs invaders
  (for* ([i (in-list player-lasers)]
        [j (in-list invaders-list)])
    ;; hit distance is the radius of both the laser and the ship added together
    (when (hit? i.x i.y j.x j.y
                (+ (/ LASER-SIZE 4) (/ INVADER-SIZE 2)))
      (+= points j.points)
      (:= explosion-list (cons (explosion j.x j.y 0) explosion-list))
      (:= invaders-list (remove j invaders-list))
      (:= player-lasers (remove i player-lasers))))

  ;; win condition
  (when (= 0 (length invaders-list))
    (println "You WIN!!!!")
      (reset (add1 current-stage)))
  
  ;; laser vs laser collisions
   (for* ([i (in-list player-lasers)]
        [j (in-list invader-lasers)])
     (when (hit? i.x i.y j.x j.y (/ LASER-SIZE 2))
       (:= player-lasers (remove i player-lasers))
       (:= invader-lasers (remove j invader-lasers))))

  ;; enemy laser vs ship collisions
  ;; ships weak spot is the green circle in the middle
  (for ([i (in-list invader-lasers)])
    (when (hit? i.x i.y
                my-ship.x
                (- my-ship.y (/ SHIP-SIZE 2))
                (+ (/ LASER-SIZE 2) (/ SHIP-SIZE 2)))
      (println "You lose!")
      (reset 0)))

  ;; explosion animation/timing
  (for ([i (in-list explosion-list)])
    (+= i.frame 1)
    (when (> i.frame 10) 
      (:= explosion-list (remove i explosion-list))))
  
  ;; draw
  (background bg-color)
  (draw-ship my-ship.x my-ship.y)
  (for ([i (in-list invaders-list)])
      (draw-invader i.x i.y))
  (for ([i (in-list player-lasers)])
      (draw-laser i.x i.y "orange"))
    (for ([i (in-list invader-lasers)])
      (draw-laser i.x i.y "Yellow"))
  (for ([i (in-list explosion-list)])
      (draw-explosion i.x i.y i.frame))
  
  (fill "orange")
  (text-align 'center 'top)
  (text-size 14)
  (text (~a "STAGE: " current-stage " SCORE: " points) (/ WIDTH 2) 10)
  
  (when paused
    (text-size 40)
    (text-align 'center 'center)
    (fill "orange")
    (text "PAUSED" (/ WIDTH 2) (/ HEIGHT 2)))
  
  (fill "orange")
  (text-align 'left 'top)
  (text-size 14)
  (text (~a " Frame-rate: " frame-rate) 40 10)
  )
