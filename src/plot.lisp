;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: D3 -*-
(in-package #:d3)

(defun plot  (x y &key (x-label "My x label") (y-label "My y label") (title "My Title") (size 7)
                            (x-minimum NIL) (y-minimum NIL) (x-maximum NIL) (y-maximum NIL)
                            (scatter-color "green") (line-color "purple") (line-width 3)
                            (stroke-fill "none") (line T) (scatter T)
                            (interpolation "Linear") (show-x-axis T) (show-y-axis T)
                            (axis-x-label-color "black") (axis-y-label-color "black")
                            (axis-x-tick_color "black") (axis-y-tick-color "black")
                            (axis-x-color "black")(axis-y-color "black") (line-opacity 1)
                            (scatter-opacity 1) (y-axis-position NIL) (x-axis-position NIL)
                            (plot-height "default") (plot-width "default")
                            (outer-background-color "none") (inner-background-color "none")
                            (annotations NIL)
                            (annotations-color "black") (annotations-font-size 12)
                            (title-font-size 18) (title-color "black")
                            (margin (list 10 10 10 10)) (padding (list 30 30 60 60))
                            (square-plot NIL) (save NIL) (svg-name "Myplot")
                            (file-name (concatenate 'string "Myplot" (write-to-string *plot-number*))))

    ;;Update plot number
    (setf *plot-number* (1+ *plot-number*))

    ;;Call function and write plot to file
    (with-open-file
      (str (concatenate 'string file-name ".html")
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
      (format str
            (htmlcode x y
                :x-label x-label :y-label y-label :title title :size size
                :x-minimum x-minimum :y-minimum y-minimum :x-maximum x-maximum :y-maximum y-maximum :scatter-color scatter-color
                :line-color line-color :line-width line-width :stroke-fill stroke-fill :line line :scatter scatter
                :interpolation interpolation :show-x-axis show-x-axis :show-y-axis show-y-axis
                :axis-x-label-color axis-x-label-color :axis-y-label-color axis-y-label-color
                :axis-x-tick_color axis-x-tick_color :axis-y-tick-color axis-y-tick-color :axis-x-color axis-x-color
                :axis-y-color axis-y-color :line-opacity line-opacity :scatter-opacity scatter-opacity
                :x-axis-position x-axis-position :y-axis-position y-axis-position
                :plot-height plot-height :plot-width plot-width
                :outer-background-color outer-background-color :inner-background-color inner-background-color :annotations annotations
                :annotations-color annotations-color :annotations-font-size annotations-font-size
                :title-font-size title-font-size :title-color title-color
                :margin margin :padding padding
                :square-plot square-plot :save save :svg-name svg-name
                :plotnum (1- *plot-number*)))))
