;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;; Interpolation examples
(let* ((x (make-list 8 :initial-element (range 0 10 :length-out 5)))
       (y (make-list 8 :initial-element (mapcar #'sin (first x)))))
      (plot x y :title "Interpolation examples" :x-label "x" :y-label "sin"
          :interpolation (list "Linear" "Step" "StepBefore" "StepAfter"
                               "Basis" "Cardinal" "MonotoneX" "CatmullRom")
          :annotations  (list (list "Linear"      1 -1.1)
                              (list "Step"        2 -1.1)
                              (list "StepBefore"  3 -1.1)
                              (list "StepAfter"   4 -1.1)
                              (list "Basis"       5 -1.1)
                              (list "Cardinal"    6 -1.1)
                              (list "MonotoneX"   7 -1.1)
                              (list "CatmullRom"  8 -1.1))
          :annotations-color (list "red" "blue" "green" "orange" "steelblue" "purple" "pink" "Gray")
          :line-color (list "red" "blue" "green" "orange" "steelblue" "purple" "pink" "Gray")
          :line-opacity 0.5 :scatter-color "black" :scatter T))
