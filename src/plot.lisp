;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;; FUNCTION FOR CREATING THE PLOT
(defun plot  (x y &key (x-label "x axis")(y-label "y axis")(title "Plot")(size 7)
                       (x-minimum NIL)(y-minimum NIL)(x-maximum NIL)(y-maximum NIL)
                       (scatter-color "green")(line-color "purple")(line-width 3)
                       (stroke-fill "none")(line T)(scatter T)
                       (interpolation "Linear")(show-x-axis T)(show-y-axis T)
                       (axis-x-label-color "black")(axis-y-label-color "black")
                       (axis-x-tick-color "black")(axis-y-tick-color "black")
                       (axis-x-color "black")(axis-y-color "black")(line-opacity 1)
                       (scatter-opacity 1)(y-axis-position NIL)(x-axis-position NIL)
                       (plot-height "default")(plot-width "default")
                       (outer-background-color "none")(inner-background-color "none")
                       (annotations NIL)(annotations-color "black")(annotations-font-size 12)
                       (title-font-size 18)(title-color "black")
                       (square-plot NIL)(save NIL)(svg-name "Myplot")
                       (margin (list 10 10 10 10))(padding (list 30 30 60 60))
		       (file-name "plot"))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating a d3js plot in an html file.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x: List (or list of lists) with values for horizontal axis plot.
    - y: List (or list of lists) with values for vertical axis plot.
    - x-label: String for the label of the horizontal axis
    - y-label: String for the label of the vertical axis
    - title:   String for the plot's title.
    - size:    Float for point (scatter) size.
    - x-minimum: Lower bound for horizontal axis on plot.
    - y-minimum: Lower bound for vertical axis on plot.
    - x-maximum: Upper bound for horizontal axis on plot.
    - y-maximum: Upper bound for vertical axis on plot.
    - scatter-color: String color for points in plot (see details)
    - line-color:    String color for points in plot (see details)
    - line-width:    Float indicating line width on plot.
    - stroke-fill:   String color to the spaces between lines (for closed curves)
    - line:          Boolean indicating whether to include lines in plot
    - scatter:       Boolean indicating whether to include points in plot.
    - interpolation: String indicating the interpolation method (see details)
    - show-x-axis:   Boolean indicating whether to include the x axis line.
    - show-y-axis:   Boolean indicating whether to include the y axis line.
    - axis-x-label-color: String color for the label of the x-axis.
    - axis-y-label-color: String color for the label of the the y-axis.
    - axis-x-tick-color:  Color for x-axis ticks.
    - axis-y-tick-color:  Color for y-axis ticks.
    - axis-x-color: String color for the x-axis.
    - axis-y-color: String color for the y-axis.
    - line-opacity: Float in [0,1] indicating the opacity of the plotted lines
    - scatter-opacity: Float in [0,1] indicating the opacity of the plotted lines
    - x-axis-position: Float y-value to center the horizontal axis.
    - y-axis-position: Float x-value to center the vertical axis.
    - plot-height: Overall height of plot (string) as CSS height (see details)
    - plot-width:  Overall width  of plot (string) as CSS width  (see details)
    - outer-background-color: Background color for outer section of plot.
    - inner-background-color: Background color for inner part of plot.
    - annotations: List of strings (see details) including text to annotate.
    - annotations-color: Font color for annotations.
    - annotations-font-size: Font size for each of the annotations.
    - title-font-size: Font size of the title.
    - title-color: Font color for the title.
    - square-plot: Force the plot to have a square shape.
    - save: Whether or not to save the plot when opening the HTML document
    - svg-name: Name for the downloaded svg file
    - margin: Plot margin size.
    - padding: Plot padding size.
    - file-name: HTML file name.

    OUTPUT:
    --------------------------------------------------------
    A file named file-name.html is created in the current
    directory. The file contains the plot and can be saved
    to svg by opening the file.

    DETAILS:
    --------------------------------------------------------

    > Multiple figures
    To plot multiple figures in the same plot create a list
    of lists with each entry belonging to a plot:
    (plot
      (list (list 1 2 3) (list 1.2 2 3.3))
      (list (list 1 2 3) (list 2 4 6)))

    > Color and fill options
    Colors can be implemented indicating the names; see
    https://bl.ocks.org/enjalot/raw/7c0fe907ba2010fed420/
    for HTML-friendly names or by indicating the HEX code.

    > Interpolation options
    Interpolation can be made the following ways:
    - Linear
    - Step
    - StepBefore
    - StepAfter
    - Basis
    - Cardinal
    - MonotoneX
    - CatmullRom
    Complete information on interpolation can be found on d3js' wiki:
    https://github.com/d3/d3-shape/blob/master/README.md#curves

    > Annotations
    annotations is a list of lists. Each element of annotations
    must have text to annotate, an x position and a y position
    to add annotations to the plot. Thus annotations must look as:
    (list (list 'someText1' xpos1 ypos1) (list 'someText2' xpos2 ypos2))

    > Additional examples
    See https://rodrigozepeda.github.io/d3lisp/index.html#examples_for_getting_started
    for additional examples.

    EXAMPLES:
    --------------------------------------------------------
    ;Plot a line:
    (plot (list 1 2 3) (list 1 2 3))
    --------------------------------------------------------

    "

    ;; Call function and write plot to file
    (with-open-file (str (translate-logical-pathname (concatenate 'string "d3:plots;"
								  file-name "-"
								  (write-to-string *plot-number*)
								  ".html"))
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
  (format str
	  (generate-html-page x y
			      :x-label x-label :y-label y-label :title title :size size
			      :x-minimum x-minimum :y-minimum y-minimum :x-maximum x-maximum
			      :y-maximum y-maximum :scatter-color scatter-color
			      :line-color line-color :line-width line-width :stroke-fill stroke-fill
			      :line line :scatter scatter
			      :interpolation interpolation :show-x-axis show-x-axis :show-y-axis show-y-axis
			      :axis-x-label-color axis-x-label-color :axis-y-label-color axis-y-label-color
			      :axis-x-tick-color axis-x-tick-color :axis-y-tick-color axis-y-tick-color
			      :axis-x-color axis-x-color :axis-y-color axis-y-color
			      :line-opacity line-opacity :scatter-opacity scatter-opacity
			      :x-axis-position x-axis-position :y-axis-position y-axis-position
			      :plot-height plot-height :plot-width plot-width
			      :outer-background-color outer-background-color
			      :inner-background-color inner-background-color
			      :annotations annotations
			      :annotations-color annotations-color
			      :annotations-font-size annotations-font-size
			      :title-font-size title-font-size :title-color title-color
			      :margin margin :padding padding :square-plot square-plot :save save
			      :svg-name svg-name :plot-number *plot-number*)))

    ;;Update plot number
    (setf *plot-number* (1+ *plot-number*)))
