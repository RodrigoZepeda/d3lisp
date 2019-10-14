;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;; Polygon museum
(let ((x1 (list 0))
      (y1 (list 0))
      (x2 (first   (ngon 3 :r 2)))
      (y2 (second  (ngon 3 :r 2)))
      (x3 (first   (ngon 4 :r 4)))
      (y3 (second  (ngon 4 :r 4)))
      (x4 (first   (ngon 5 :r 6)))
      (y4 (second  (ngon 5 :r 6)))
      (x5 (first   (ngon 6 :r 8)))
      (y5 (second  (ngon 6 :r 8)))
      (x6 (first   (ngon 7 :r 10)))
      (y6 (second  (ngon 7 :r 10)))
      (x7 (first   (ngon 8 :r 12)))
      (y7 (second  (ngon 8 :r 12)))
      (x8 (first   (ngon 9 :r 14)))
      (y8 (second  (ngon 9 :r 14)))
      (x9 (first   (ngon 10 :r 16)))
      (y9 (second  (ngon 10 :r 16)))
      (x10 (first  (ngon 11 :r 18)))
      (y10 (second (ngon 11 :r 18)))
      (x11 (first  (ngon 12 :r 20)))
      (y11 (second (ngon 12 :r 20))))

      ;Creation of all the poligons
      (plot (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
                  (list y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11)
                  :title "Regular poligons museum" :scatter T
                  :line-color (list "#ffa190" "#ff917e" "#ff826b" "#ff7259" "#ff6347"
                                   "#e5593f" "#cc4f38" "#b24531" "#993b2a" "#7f3123" "#66271c")
                  :size 4
                  :title-font-size 25
                  :title-color "white"
                  :line-width 4
                  :padding (list 50 30 60 50)
                  :x-minimum -20 :x-maximum 20 :y-minimum -20 :y-maximum 20
                  :scatter-color "white"
                  :outer-background-color "black"
                  :line-color "red"
                  :interpolation "Linear"
                  :show-x-axis NIL :show-y-axis NIL :square-plot T))
