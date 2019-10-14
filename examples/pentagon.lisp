;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;; Regular ngon
(let ((x (first (ngon 5)))
      (y (second (ngon 5))))
      (plot x y :title "Pentagon" :square-plot T :stroke-fill "#47476b"
                      :line-color "black" :padding (list 75 75 60 60)
                      :title-font-size 50 :scatter NIL :square-plot T
                      :show-x-axis NIL :show-y-axis NIL :save T :svg-name "pentagon"))
