;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;; Exponential example using range
(let* ((x (range 0 5 :length-out 10))
       (y (mapcar #'exp x)))
      (plot x y :title "Exponential" :square-plot T :title-font-size 100
                :margin (list 10 10 10 10) :padding (list 150 30 60 60)
                :x-label "x" :y-label "exp(x)" :interpolation "MonotoneX"
                :scatter-color "red" :line-color "black"))
