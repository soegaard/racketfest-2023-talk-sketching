#lang racket
(provide
 standard-passes-pict
 adjust-passes-pict)

(require metapict metapict/crop)

(set-curve-pict-size 800 50)
(curve-pict-window (window 0 800 -25 25))
(ahlength (px 4))
(current-label-gap (px 4))
(current-neighbour-distance 70)
(current-inner-separation    3)

(def n1 (rectangle-node "Source"              #:at (pt 50 0)))
(def n2 (rectangle-node "Syntax Object"       #:right-of n1))
(def n3 (rectangle-node "Syntax Object"       #:right-of n2))
(def n4 (rectangle-node "Compiled Expression" #:right-of n3))
(def n5 (text-node "" #:right-of n4))

(def e1 (edge n1 n2 #:label "read"))                    
(def e2 (edge n2 n3 #:label "expand"))
(def e3 (edge n3 n4 #:label "compile"))
(def e4 (edge n4 n5 #:label "eval"))

(define standard-passes-pict
  (crop/inked
   (draw n1 n2 n3 n4 
         e1 e2 e3 e4)))


(def m1 (rectangle-node "Source"              #:at (pt 50 0)))
(def m2 (rectangle-node "Syntax Object"       #:right-of m1))
(def m3 (rectangle-node "Syntax Object"       #:right-of m2))
(def m4 (rectangle-node "Syntax Object"       #:right-of m3))
(def m5 (rectangle-node "Compiled Expression" #:right-of m4))
(def m6 (text-node "" #:right-of m5))

(def f1 (edge m1 m2 #:label "read"))                    
(def f2 (edge m2 m3 #:label "adjust"))
(def f3 (edge m3 m4 #:label "expand"))
(def f4 (edge m4 m5 #:label "compile"))
(def f5 (edge m5 m6 #:label "eval"))

(define adjust-passes-pict
  (crop/inked
   (draw m1 m2 m3 m4 m5
         f1 f2 f3 f4 f5)))
