#lang at-exp slideshow/widescreen
;;;
;;; Sketching - RacketFest 2023 Berlin 
;;; - Jens Axel Søgaard
;;;

(require (except-in metapict text open)
         metapict/crop
         metapict/path-operations
         metapict/polygons
         racket/match
         slideshow/code)

;;;
;;; General Utilities
;;;

(define aspect-default 'widescreen)

(define (slide* #:title     [title  #f]
                #:name      [name   #f]
                #:aspect    [aspect aspect-default]
                #:layout    [layout 'auto]
                #:gap-size  [sep-gap-size (current-gap-size)]
                #:inset     [inset  (make-slide-inset 0 0 0 0)]
                #:timeout   [secs   #f]
                #:condense? [condense? (and secs #t)]
                items)
  (keyword-apply slide ; keywords are sorted
                 '(  #:aspect #:condense?   #:gap-size #:inset #:layout #:name #:timeout #:title)
                 (list aspect   condense? sep-gap-size   inset   layout   name   secs      title)
                 items))

(define (keynote-scale x) (inexact->exact (floor (* 0.75 x))))

;;;
;;; Layout
;;;

(define font         "Helvetica Neue")
(define font-regular (~a font ", Light"))
(define font-medium  (~a font ", Medium"))

; The main font is used by `t`.
(current-main-font font-regular)
(current-font-size (keynote-scale 48)) ; pt ?

; A title is shown at the top of most slides
(define title-size (keynote-scale 112))
(define title-font font-medium)

(define title-small-size (keynote-scale 84))
(define title-small-font font-medium)

(define subtitle-size (keynote-scale 54))
(define subtitle-font font-regular)


; Used by `titlet`.
(current-titlet
 ; A single string becomes a title.
 ; A list of two strings becomes a title and a subtitle.
 (λ (s)
   (match s
     [(? string? s)         (colorize            (text s title-font title-size)
                                                 (current-title-color))]
     [(list title subtitle) (colorize (vc-append (text title    title-font    title-size)
                                                 (text subtitle subtitle-font subtitle-size))
                                      (current-title-color))])))

(define (scale-to-width pict width)
  (define w (pict-width pict))
  (scale (/ width w) pict))

;;;
;;; Master Slides (templates)
;;;

(define (title-and-subtitle title subtitle)
  ; Used for the first slide. 
  (slide
   (text    title    title-font    title-size)
   (text subtitle subtitle-font subtitle-size)))

(define (title-and-subtitle/background title subtitle background)
  ; Used for the first slide. 
  (slide* #:title (list title subtitle)
          (list background)))

(define (title-subtitle-and-text
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title subtitle . items)
  (slide* #:title (list title subtitle)
          (list (para #:aspect aspect-default
                      #:align   align
                      #:fill?   fill?
                      #:decode? decode?
                      items))))
  

(define (title-and-bullets title . items)
  (slide* #:title title items))

(define (title-centre title)
  (slide
   (text title title-font title-size)))

(define (photo-fullscreen photo)
  (slide
   (para #:fill? #t #:align 'center
         (scale-to-width photo (current-para-width)))))

(define (two-photos 
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         photo1 photo2)

  (parameterize ([current-para-width (/ (- client-w (* 4 (current-gap-size))) 2)])
    (slide
     (hc-append
      (para #:fill? #t #:align 'center
            (scale-to-width photo1 (current-para-width)))
      (blank (current-gap-size) 0)
      (para #:fill? #t #:align 'center
            (scale-to-width photo2 (current-para-width)))))))


(define (title-and-photo 
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title photo)
  (slide
   (vc-append
    (parameterize ([current-main-font title-small-font]
                   [current-font-size title-small-size])
      (para #:aspect aspect-default
            #:align   align
            #:fill?   fill?
            #:decode? decode?
            title))
    (blank 0 (current-gap-size))
    (para #:fill? #t #:align 'center
          (scale-to-width photo (current-para-width))))))


(define (photo-vertical
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title-small subtitle photo)
  ; Title and subtitle to the left of photo (pict)
  (parameterize ([current-para-width (/ (- client-w (* 4 (current-gap-size))) 2)])
    (slide
     (hc-append
      (vc-append
       (parameterize ([current-main-font title-small-font]
                      [current-font-size title-small-size])
         (para #:aspect aspect-default
               #:align   align
               #:fill?   fill?
               #:decode? decode?
               title-small))
       (parameterize ([current-main-font subtitle-font]
                      [current-font-size subtitle-size])
         (para #:aspect aspect-default
               #:align   align
               #:fill?   fill?
               #:decode? decode?
               subtitle)))
      (para #:fill? #t #:align 'center
            (scale-to-width photo (current-para-width)))))))



(define (title-bullets-and-photo
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title photo . items)
  (define half-width (/ (- client-w (* 4 (current-gap-size))) 2))
  (parameterize ([current-para-width half-width])
    (slide #:title title           
           (hc-append
            (apply vl-append
                   (for/list ([item-text items])
                     (item #:width (- half-width (* 2 (current-gap-size)))
                           (para item-text))))
            (scale-to-width photo (current-para-width))))))


(define (title-code-and-pict
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title code-pict photo)
  (define half-width (/ (- client-w (* 4 (current-gap-size))) 2))
  (parameterize ([current-para-width half-width])
    (slide #:title title           
           (hc-append
            (scale-to-width code-pict (current-para-width))
            (blank (* 2 (current-gap-size)))
            (scale-to-width photo (- (current-para-width) (* 2 (current-gap-size))))))))

(define (title-text-code-and-pict
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title text code-pict photo)
  (define half-width (/ (- client-w (* 4 (current-gap-size))) 2))
  (parameterize ([current-para-width half-width])
    (slide #:title title           
           (hc-append
            (vl-append
             (para text #:fill? #t #:width half-width #:decode? #f)
             (blank 0 (current-gap-size))
             (scale-to-width code-pict (current-para-width)))
            (blank (* 2 (current-gap-size)))
            (scale-to-width photo
                            (min (- (current-para-width) (* 2 (current-gap-size)))
                                 (- client-h (* (current-gap-size)
                                                (if (list? title)
                                                    10
                                                    4)))))))))

(define (title-text-code-and-code
         #:align   [align   'center] ; left, right
         #:decode? [decode? #t]
         #:fill?   [fill?   #t]
         title text1 text2 code-pict1 code-pict2)
  (define half-width (/ (- client-w (* 4 (current-gap-size))) 2))
  (parameterize ([current-para-width half-width])
    (slide #:title title           
           (ht-append
            (vl-append
             (para text1 #:fill? #t #:width half-width #:decode? #f)
             (blank 0 (current-gap-size))
             (scale-to-width code-pict1 (current-para-width)))
            (blank (* 2 (current-gap-size)))
            (vl-append
             (para text2 #:fill? #t #:width half-width #:decode? #f)
             (blank 0 (current-gap-size))
             (scale-to-width code-pict2 (current-para-width)))))))

;;;
;;; Slides for Sketching talk at RacketFest 2023 in Berlin.
;;;

(set-curve-pict-size 400 400)



(title-and-subtitle/background
 "Sketching"
 "Jens Axel Søgaard, 2023"
 (inset (scale 0.3 (bitmap "images/racketfest-logo.png")) 200 0))

#;(title-and-subtitle "Sketching"
                    "Jens Axel Søgaard, 2023")


(title-bullets-and-photo
 "What is Sketching?"
 (inset (bitmap "images/nut-shell.png") 200 0)
 "language/library for creative programming"
 "make graphical programs accessible for beginners, artists and educators.")

;; Inspiration
(photo-fullscreen
 (bitmap "images/the-coding-train-all-aboard.png"))

(title-and-photo
 "Daniel Schiffman"
 (bitmap "images/daniel-schiffman.png"))

(title-and-photo
 "Coding Challenges"
 (bitmap "images/coding-challenges.png"))

(two-photos 
 (bitmap "images/welcome-to-processing.png")
 (bitmap "images/large-processing-logo.png"))

(title-and-photo
 "Ben Fry and Casey Reas"
 (bitmap "images/ben-fry-and-casey-reas.png"))

#;(title-bullets-and-photo
 "What is Sketching?"
 (inset (bitmap "images/nut-shell.png") 200 0)
 "language/library for creative programming"
 "make graphical programs accessible for beginners, artists and educators.")


(title-code-and-pict
 "The first sketch"
 (codeblock-pict
  @~a{#lang sketching
      
      (define (setup)
        (size 640 360))
      
      (define (draw)
        (rect (random width) (random height) 100 100))})

 (frame (bitmap "images/rectangles0.png")))

(title-code-and-pict
 "The first sketch II"
 (codeblock-pict
  @~a{#lang sketching
      
      (define (setup)
        (size 640 360))
      
      (define (draw)
        (fill (random 256))
        (rect (random width) (random height) 100 100))})

 (frame (bitmap "images/rectangles1.png")))

(title-code-and-pict
 "The first sketch III"
 (codeblock-pict
  @~a{#lang sketching
      
      (define (setup)
        (size 640 360)
        (color-mode 'hsb 100 100 100))
      
      (define (draw)
        (fill (random 100) 100 100)
        (rect (random width) (random height) 100 100))})

 (frame (bitmap "images/rectangles2.png")))

(title-code-and-pict
 "The first sketch IV"
 (codeblock-pict
  @~a{#lang sketching
      
      (define (setup)
        (size 640 360)
        (color-mode 'hsb 100 100 100))
      
      (define (draw)
        (fill (random 100) 100 100)
        (rect mouse-x mouse-y 100 100))})

 (frame (bitmap "images/rectangles3.png")))

(title-code-and-pict
 "The first sketch IV"
 (codeblock-pict
  @~a{#lang sketching
      
      (define t 0)

      (define (setup)
        (size 640 360)
        (color-mode 'hsb 100 100 100))

      (define (draw)
        (:= t (+ t 1))
        (fill t 100 100)
        (rect mouse-x mouse-y 100 100))})

 (frame (bitmap "images/rectangles4.png")))


(title-and-subtitle/background 
 "Projects" ""
 (inset (scale 0.5 (bitmap "images/new-framework.png")) 200 0))

(title-and-subtitle/background 
 "Space Invaders" 
 "https://github.com/hasn0life/sketching-space-invaders"
 (inset (scale 0.4 (bitmap "images/space-invaders.png")) 200 0))

(title-and-subtitle/background 
 "Asteroids" 
 "https://github.com/hasn0life/asteroids-sketching-demo"
 (inset (scale 0.4 (bitmap "images/asteroids.png")) 200 0))

(title-and-subtitle/background 
 "Getting started with Sketching"
 "Eric Cervin"
 (bitmap "images/getting-started-robot.png"))

(title-code-and-pict
 "Getting started with Sketching"
 (codeblock-pict
  @~a{(define speed 0.005)

      (struct state (angle angle-direction))

      (define s (state 0.0 1))

      (define (update-state)
        (+= s.angle (* s.angle-direction speed))
        (when (or (> s.angle pi/4)
                  (< s.angle 0))
          (*= s.angle-direction -1)))

      (define (draw)
        (background 204)
        (translate 20 25) ;;move to start position           ;
        (rotate s.angle)
        (stroke-weight 12)
        (line 0 0 40 0)
        (translate 40 0) ;; move to next joint
        (rotate (* s.angle 2.0))
        (stroke-weight 6)
        (line 0 0 30 0)
        (translate 30 0);; move to next joint
        (rotate (* s.angle 2.5))
        (stroke-weight 3)
        (line 0 0 20 0)
        (update-state))})

 (frame (inset (bitmap "images/getting-started-robot-arm.png") 300 300)))



(title-code-and-pict
 "Chaos Game - Timo Netzer" ;  (eXodiquas)
 (codeblock-pict
  @~a{(define (draw)
        ; press any key while having at least 2 fixed 
        ; points on the screen to unpause.
        (when (and key-pressed
                   (> (length fixed-points) 1))
          (:= current-point
              (let-values ([(p1 p2)
                            (choose-two-points fixed-points)])
                (point-midpoint p1 p2)))
          (:= paused #f))

        ; add a fixed point at the mouse position to the screen.
        (when (mouse-pressed)
          (:= fixed-points
              (cons (point mouse-x mouse-y) fixed-points)))

        ; draw all the fixed points
        (for ([p fixed-points])
          (fix-point-draw p)))})
 
 (frame (inset (bitmap "images/chaos-game.png") 100 100)))


(title-code-and-pict
 ; https://raw.githubusercontent.com/Bracktus/bits-n-bobs/main/games/rounders.rkt
 "Rounders - Ricky C. (Bracktus)"
 (codeblock-pict
  @~a{(class Ball Object
        (init-field x y r)
        (super-new)
        (define/public (render)
          (stroke 0 255 0)
          (no-fill)
          (circle x y r)))
      
      (define/public (render)
        (for ([ball ball-lst])
          (ball.render)))})
 (bitmap "images/rounders.png"))





(title-centre "Processing vs Sketching")


(title-text-code-and-code
 "Comparison - Color Bars"
 "Processing" "Sketching"
 (codeblock-pict
  @~a{int barWidth = 20;
      int lastBar = -1;

      void setup() {
           size(640, 360);
           colorMode(HSB, height, height, height);  
           noStroke();
           background(0);
      }

      void draw() {
          int whichBar = mouseX / barWidth;
          if (whichBar != lastBar) {
              int barX = whichBar * barWidth;
              fill(mouseY, height, height);
              rect(barX, 0, barWidth, height);
              lastBar = whichBar;
           }
      }})
 (codeblock-pict
  @~a{#lang sketching
      (define bar-width 20)
      (define last-bar  -1)

      (define (setup)
        (size 640 360)  
        (color-mode 'hsb height height height)
        (no-stroke)
        (background 0))
      
      (define (draw)
        (define which-bar (quotient mouse-x bar-width))
        (unless (= which-bar last-bar)
          (define bar-x (* which-bar bar-width))
          (fill mouse-y height height)
          (rect bar-x 0 bar-width height)
          (:= last-bar which-bar)))}))


(title-text-code-and-code
 "Comparison - Color Bars"
 "Processing" "Sketching"
 (codeblock-pict
  @~a{int barWidth = 20;
      int lastBar = -1;

      void setup() {
           size(640, 360);
           colorMode(HSB, height, height, height);  
           noStroke();
           background(0);
      }

      void draw() {
          int whichBar = mouseX / barWidth;
          if (whichBar != lastBar) {
              int barX = whichBar * barWidth;
              fill(mouseY, height, height);
              rect(barX, 0, barWidth, height);
              lastBar = whichBar;
           }
      }})
 (codeblock-pict
  @~a{#lang sketching
      (define bar-width 20)
      (define last-bar  -1)

      (define (setup)
        (size 640 360)  
        (color-mode 'hsb height height height)
        (no-stroke)
        (background 0))
      
      (define (draw)
        (define which-bar (quotient mouse-x bar-width))
        (unless (= which-bar last-bar)
          (define bar-x (* which-bar bar-width))
          (fill mouse-y height height)
          (rect bar-x 0 bar-width height)
          (:= last-bar which-bar)))}))


(title-and-photo "Vector notation"
   (inset (bitmap "images/storing-input.png")
          400 0))

(title-text-code-and-code
 "Comparison - Vector Notation"
 "Processing" "Sketching"
 (codeblock-pict
  @~a{
int num = 60;
float mx[] = new float[num];
float my[] = new float[num];

void setup() {
  size(640, 360);
  noStroke();
  fill(255, 153); 
}

void draw() {
  background(51); 
  
  int which = frameCount % num;
  mx[which] = mouseX;
  my[which] = mouseY;
  
  for (int i = 0; i < num; i++) {
    int index = (which+1 + i) % num;
    ellipse(mx[index], my[index], i, i);
  }                                               ;
}})
 (codeblock-pict
  @~a{#lang sketching

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
          (define index (modulo (+ which 1 i) n))      ;
          (ellipse mx[index] my[index] i i)))
      }))


(title-text-code-and-code
 "Comparison - Spot"
 "Processing" "Sketching"
 (codeblock-pict
  @~a{
class Spot {
  float x, y, radius;
  Spot() {
    radius = 40;
    x = width*0.25;
    y = height*0.5;
  }
  Spot(float xpos, float ypos, float r) {
    x = xpos;
    y = ypos;
    radius = r;
  }
  void display() {
    ellipse(x, y, radius*2, radius*2);
  }

Spot sp1, sp2;

void setup() {
  size(640, 360);
  background(204);
  noLoop();
  sp1 = new Spot();
  sp2 = new Spot(width*0.5, height*0.5, 120);                    ;
}

void draw() {
  sp1.display();
  sp2.display();
}

  
}})
 (codeblock-pict
  @~a{#lang sketching
      (class Spot Object
        (init-field
         [x      (* width 0.25)]
         [y      (* height 0.5)]
         [radius 40])  
        ; "Constructor"  
        (super-new)
        ; Methods
        (define/public (display)    
          (ellipse x y (* radius 2) (* radius 2))))

      (define sp1 #f)
      (define sp2 #f)

      (define (setup)
        (size 640 360)
        (background 204)
        (no-loop)
        (:= sp1 (make-object Spot))
        (:= sp2 (make-object Spot (* width 0.5) (* height 0.5) 120)))

      (define (draw)
        (sp1.display)
        (sp2.display))
      }))

(title-code-and-pict
 ; https://github.com/soegaard/sketching/tree/main/sketching-examples/examples/pacman
 "Untitled Maze Game"
 (inset
 (codeblock-pict
  @~a{
(define level-data
  (vector
   ;0   4   8   2   6   0   4   8
   "╔════════════╕╒════════════╗"
   "║............║║............║"
   "║.┌──┐.┌───┐.║║.┌───┐.┌──┐.║"
   "║*│  │.│   │.║║.│   │.│  │*║"
   "║.└──┘.└───┘.╚╝.└───┘.└──┘.║"
   "║..........................║"
   "║.┌──┐.┌┐.┌──────┐.┌┐.┌──┐.║"
   "║.└──┘.││.└──┐┌──┘.││.└──┘.║"
   "║......││....││....││......║"
   "╚════╗.│└──┐ └┘ ┌──┘│.╔════╝"
   "     ║.│┌──┘    └──┐│.║     "
   "     ║.││          ││.║     "
   "     ║.││ ╔______╗ ││.║     "
   "═════╝.└┘ ║      ║ └┘.╚═════"
   "Ttttt .   ╚══════╝   .tttttT"
   "═════╗.┌┐ ┌──────┐ ┌┐.╔═════"
   "     ║.││ └──┐┌──┘ ││.║     "
   "╔════╝.└┘    ││    └┘.╚════╗"
   "║............││............║"
   "║.┌──┐.┌───┐.└┘.┌───┐.┌──┐.║"
   "║.└─┐│.│   │....│   │.│┌─┘.║"
   "║*..││.│   └────┘   │.││..*║"
   "╚═╗.││.│            │.││.╔═╝"
   "╔═╝.└┘.└────────────┘.└┘.╚═╗"
   "║..........................║"
   "╚══════════════════════════╝"))})
 300 0)
 (inset (bitmap "images/pacman.png")
        300 0))


(title-centre "Implementation")

(require "block-diagrams.rkt")

(title-subtitle-and-text 
 "Implementation"
 "Indexing for vectors, strings and hash tables"

 (vl-append
  (codeblock-pict "id[expr ...]")
  (blank 0 (* 4 (current-gap-size)))
  (codeblock-pict "(#%ref id expr ...)")))


(title-subtitle-and-text 
 "Implementation"
 "Vector Indexing"
 (vl-append
  standard-passes-pict
  (blank 0 (* 5 (current-gap-size)))
  adjust-passes-pict))


(title-and-photo "Implementation"
 (codeblock-pict
  @~a{
(module reader syntax/module-reader
  ; 1. Module path of the language.
  sketching/main
  ;   The module path `sketching/main` is used in the language position
  ;   of read modules. That is, reading `#lang sketching` will produce a
  ;   module with `sketching/main` as language.

  ; 2. Reader options (#:read, #:read-syntax, etc. ...)
  #:module-wrapper (λ (thunk) (adjust (thunk)))

  ...
  )}))

(title-and-photo "Implementation - Indexing"
 (codeblock-pict
  @~a{
  (define (adjust stx)
    (syntax-parse stx
      [(a . d) (adjust-dotted-list stx)]
      [_       stx]))
  
  (define (adjust-dotted-list stx)    
    (syntax-parse stx
      [(id:id (~and [e:expr ...] brackets)  . more)
       (cond
         [(and (eqv? (syntax-property #'brackets 'paren-shape) #\[)
               (= (+ (syntax-position #'id) (syntax-span #'id))
                  (syntax-position #'brackets)))
          (let ([adjusted-more (adjust #'more)]
                [arguments     (syntax->list #'(id e ...))])
            (datum->syntax #f
                           `((#%ref ,@"@"arguments) . ,adjusted-more)
                           stx))]
         [else
          (with-syntax ([(_ . rest) stx])
            (let ([adjusted-rest (adjust-dotted-list #'rest)])
              (datum->syntax #f
                             `(,#'id . ,adjusted-rest)
                             stx)))])]
      [(a . more)
       (let ([adjusted-a    (adjust #'a)]
             [adjusted-more (adjust #'more)])
         (datum->syntax #f
                        `(,adjusted-a . ,adjusted-more)
                        stx))]
      [_
       (raise-syntax-error 'adjust-dotted-list "expected a dotted list" stx)]))                       ;
  }))


(title-subtitle-and-text
 "Questions?" ""
 (t "Thanks to Stephen De Gabrielle for arranging Creative Racket"))



;;;
;;; The slides below wasn't presented at RacketFest.
;;;


#;(title-and-photo "#lang sketching"
 (vl-append
  (para "The user writes a standard module with expressions and definition in some order.")
  (blank (current-gap-size))
  (codeblock-pict
   @~a{
       #lang sketching
       ...
       (define (draw) ...)
       (define (setup) ...)})
  (blank (current-gap-size))
  (para "Our " (tt "module-begin") " turns this into:")
  (blank (current-gap-size))
  (codeblock-pict
   @~a{
       #lang sketching
       ...
       (define (draw) ...)
       (define (setup) ...)
       (setup) (current-draw draw) (start)})))

#;(title-and-photo "#%top"
  (codeblock-pict
   @~a{
; SYNTAX  (sketching-top . id)
;   Like #%top, but additionally:
;     - identifiers containing a dot is rewritten to use dot-field,
;       enabling  (#%app foo.bar 1 2) -> (send foo bar 1 2 )
;       and       foo.bar             -> (get-field foo bar)
;     - default values for the event handlers:
;          on-mouse-pressed, on-key-pressed, etc.
;     - default values for  setup  and  draw
(define-syntax (sketching-top stx)
  (syntax-parse stx
    [(top . top-id)
     (with-syntax ([default-setup (datum->syntax #'top 'default-setup)]
                   [default-draw  (datum->syntax #'top 'default-draw)])
       (case (syntax->datum #'top-id)
         [(setup)             #'default-setup]
         [(draw)              #'default-draw]
         [(on-mouse-pressed)  #'#f] ; etc
         [else
          (cond
            [(or (id-contains? #'top-id ".") (id-contains? #'top-id "_"))
             (with-syntax ([(id ...) (map number-id->number 
                                          (split-id-at-dot/underscore #'top-id))])
               (syntax/loc stx (dot/underscore id ...)))]
            [else
             #'(#%top . top-id)])]))]))}))


#;(title-and-photo "#%app"
  (codeblock-pict
   @~a{
; SYNTAX  (sketching-app proc-expr arg ...)
;   Like #%app, but additionally:
;     - a call to  o.m  where o is an object and m is a method name,
;       is rewritten to a method call
;     - a call to  f, where the identifier f contains no dots,
;       is a normal call.
(define-syntax (sketching-app stx)
  (syntax-parse stx
    ; calls to a   o.m is a method call
    ; calls to an  f   is a normal call
    [(app proc-id:id . args)
     (define str (symbol->string (syntax-e #'proc-id)))
     (cond
       [(string-contains? str ".")
        (define ids (map string->symbol (string-split str ".")))
        (with-syntax ([(id ...) (for/list ([id ids])
                                  (datum->syntax #'proc-id id))])
          #'(app-dot-method (id ...) . args))]
       [else
        #'(#%app proc-id . args)])]
    [(app . more)
     (syntax/loc stx
       (#%app . more))]))
}))





;; #lang sketching

;; (define (setup)
;;   (size 640 360)  
;;   (color-mode 'hsb 100 100 100))

;; (define (draw)
;;   (rect-mode 'center)
;;   (fill (+ mouse-x mouse-y) 100 100)
;;   (rect mouse-x mouse-y 100 100))


;;;

; Daniel Shiffman (born July 29, 1973) is a computer programmer,
; a member of the Board of Directors of the Processing Foundation,[1]
; and an Associate Arts Professor at the Interactive Telecommunications Program (ITP)
; at New York University Tisch School of the Arts.[2]
; Shiffman received a BA in Mathematics and Philosophy from Yale University
; and a master's degree from the ITP.[3]

;; Ben Fry and Casey Reas started Processing in Spring 2001 and continue
;; to obsessively work on it. In 2012, they started the Processing
;; Foundation along with Dan Shiffman, who formally joined as a third
;; project lead.

;; Ben Fry is principal of Fathom, a design and software consultancy
;; located in Boston. He received his doctoral degree from the Aesthetics
;; + Computation Group at the MIT Media Laboratory, where his research
;; focused on combining fields such as computer science, statistics,
;; graphic design, and data visualization as a means for understanding
;; information.

;; Reas' software, prints, and installations have been featured in
;; numerous solo and group exhibitions at museums and galleries in the
;; United States, Europe, and Asia. His work ranges from small works on
;; paper to urban-scale installations, and he balances solo work in the
;; studio with collaborations with architects and musicians. Reas' work
;; is in a range of private and public collections, including the Centre
;; Georges Pompidou and the San Francisco Museum of Modern Art. 

;; Reas is a
;; professor at the University of California, Los Angeles. He holds a
;; master's degree from the Massachusetts Institute of Technology in
;; Media Arts and Sciences and a bachelor's degree from the College of
;; Design, Architecture, Art, and Planning at the University of
;; Cincinnati. With Ben Fry, Reas initiated Processing in 2001;
;; Processing is an open-source programming language and environment for
;; the visual arts.

(set-curve-pict-size 400 400)
