;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: D3 -*-
(in-package #:d3)

(defun createplot  (x y &key (xlab "My x label") (ylab "My y label") (title "My Title") (size 7)
                            (xmin NIL) (ymin NIL) (xmax NIL) (ymax NIL) (scatter-color "green")
                            (line-color "purple") (line-width 3) (stroke-fill "none") (line T) (scatter T)
                            (interpolation "Linear") (showXaxis T) (showYaxis T)
                            (axisX_label_color "black") (axisY_label_color "black")
                            (axisX_tick_color "black") (axisY_tick_color "black") (axisX_color "black")
                            (axisY_color "black") (line-opacity 1) (scatter-opacity 1)
                            (yaxispos NIL) (xaxispos NIL)
                            (plotheight "default") (plotwidth "default")
                            (outercolor "none") (innercolor "none") (annotations NIL)
                            (annotations-color "black") (annotations-font-size 12)
                            (title-fontsize 18) (title-color "black")
                            (margin (list 10 10 10 10)) (padding (list 30 30 60 60))
                            (squareplot NIL) (save NIL) (svgname "Myplot")
                            (filename (concatenate 'string "Myplot" (write-to-string *plot-number*))))

    ;;Update plot number
    (setf *plot-number* (1+ *plot-number*))

    ;;Call function and write plot to file
    (with-open-file
      (str (concatenate 'string filename ".html")
           :direction :output
           :if-exists :supersede
           :if-does-not-exist :create)
      (format str
            (htmlcode x y
                :xlab xlab :ylab ylab :title title :size size
                :xmin xmin :ymin ymin :xmax xmax :ymax ymax :scatter-color scatter-color
                :line-color line-color :line-width line-width :stroke-fill stroke-fill :line line :scatter scatter
                :interpolation interpolation :showXaxis showXaxis :showYaxis showYaxis
                :axisX_label_color axisX_label_color :axisY_label_color axisY_label_color
                :axisX_tick_color axisX_tick_color :axisY_tick_color axisY_tick_color :axisX_color axisX_color
                :axisY_color axisY_color :line-opacity line-opacity :scatter-opacity scatter-opacity
                :xaxispos xaxispos :yaxispos yaxispos
                :plotheight plotheight :plotwidth plotwidth
                :outercolor outercolor :innercolor innercolor :annotations annotations
                :annotations-color annotations-color :annotations-font-size annotations-font-size
                :title-fontsize title-fontsize :title-color title-color
                :margin margin :padding padding
                :squareplot squareplot :save save :svgname svgname
                :plotnum (1- *plot-number*)
            )
      )
    )



)
