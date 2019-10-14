;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;FUNCTION THAT TRANSFORMS LISP SEQUENCE TO JAVASCRIPT ARRAY
(defun to-javascript-array (x)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for transforming a numerical LISP SEQUENCE to a
    Javascript array
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x: A LISP numerical SEQUENCE

    OUTPUT:
    --------------------------------------------------------
    - javascript-array: A Javascript array containing the
    elements of x

    EXAMPLES:
    --------------------------------------------------------
    ;Create an array with numbers 1, 2 and 3
    (to-javascript-array (list 1 2 3))

    ; Create an array from a vector
    (to-javascript-array #(1 2 3))

    ;List elements must be numeric
    (to-javascript-array (list 'a' 2 3))
    --------------------------------------------------------

    "
  (declare (sequence x))
  (with-output-to-string (jsa)
    (write-string "[" jsa)
    (etypecase x
      (vector (loop for e across x
		 do (format jsa "~A" e) ; Not ~F because JavaScript does not handle scientific notation (?)
		 unless (eq e (last-elt x)) do (format jsa ","))) 
      (list   (format jsa "~{~A~^,~}" x))) ; Sadly, format iteration does not work on vectors
    (write-string "]" jsa)))

;;FUNCTION THAT INSTANTIATES JAVASCRIPT VARIABLES FOR PLOTTING
(defun instantiate-javascript
    (x y &key
        (scatter         (make-list (length x) :initial-element "true"))
        (line            (make-list (length x) :initial-element "true"))
        (size            (make-list (length x) :initial-element 5))
        (line-width      (make-list (length x) :initial-element 3))
        (line-opacity    (make-list (length x) :initial-element 1))
        (scatter-opacity (make-list (length x) :initial-element 1))
        (scatter-color   (make-list (length x) :initial-element "red"))
        (line-color      (make-list (length x) :initial-element "blue"))
        (stroke-fill     (make-list (length x) :initial-element "none"))
        (interpolation   (make-list (length x) :initial-element "Linear")))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for generating a string for instantiating
    javascript variables of the form:

    var xdata           = [1,2,3],
        ydata           = [4,5,6],
        scatter         = 'true',
        line            = 'true',
        radii           = 5,
        line-width      = 3,
        line-opacity    = 1,
        scatter-opacity = 1,
        scatter-color   = 'red',
        line-color      = 'blue',
        stroke-fill     = 'none',
        plotcurve       =  d3.curveLinear;

    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x:               List of lists of variables to plot on x-axis
    - y:               List of lists of variables to plot on y-axis
    - scatter:         Boolean indicating whether or not to do scatter plot
    - line:            Boolean indicating whether or not to do add lines to plot
    - size:            Positive float indicating the scatter size.
    - line-width:      Positive float indicating the line width.
    - line-opacity:    Positive float in [0,1] indicating the line opacity.
    - scatter-opacity: Positive float in [0,1] indicating the line opacity
    - scatter-color:   String indicating the scatter color. See DETAILS for options.
    - line-color:      String indicating the line color. See DETAILS for options.
    - stroke-fill:     String indicating either the stroke fill color or 'none'
    - interpolation:   String of interpolation method for line joining scatters. See DETAILS.


    OUTPUT:
    --------------------------------------------------------
    - javascript-string: A string for instantiationg
    necessary javascript variables as specified in
    DESCRIPTION

    DETAILS:
    --------------------------------------------------------

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

    EXAMPLES:
    --------------------------------------------------------
    ;Create javascript instantiation for ploting (1,2,3) (1,2,3)
    (instantiate-javascript (list (list 1 2 3)) (list (list 1 2 3)))

    ;Create javascript instantiation for two plots
    (instantiate-javascript (list (list 1 2 3) (list 1 3 7))
                            (list (list 1 2 3) (list 1 9 49)))

    ;Create javascript instantiation for two plots with multiple options
    (instantiate-javascript (list (list 1 2 3) (list 1 3 7))
                            (list (list 1 2 3) (list 1 9 49))
                            :size (list 8 6)
                            :line-width (list 1 1))

    --------------------------------------------------------

    "
    (let ((javascript-string ""))
        (loop for i from 0 to (1- (length x))
            do (progn
                (setf javascript-string
                  ;This creates a javascript instantiation of variables of the form
                  ;var myvariable     = myvalue,
                  ;    othervariable  = othervalue;
                    (concatenate 'string javascript-string
                        "var xdata"      (write-to-string i) "=" (to-javascript-array (nth i x)) ","
                        "ydata"          (write-to-string i) "=" (to-javascript-array (nth i y)) ","
                        "scatter"        (write-to-string i) "=" (nth i scatter) ","
                        "line"           (write-to-string i) "=" (nth i line) ","
                        "radii"          (write-to-string i) "=" (write-to-string
                                            (coerce (nth i size) 'single-float)) ","
                        "linewidth"     (write-to-string i) "=" (write-to-string
                                            (coerce (nth i line-width) 'single-float)) ","
                        "lineopacity"   (write-to-string i) "=" (write-to-string
                                            (coerce (nth i line-opacity) 'single-float)) ","
                        "scatteropacity"(write-to-string i) "=" (write-to-string
                                            (coerce (nth i scatter-opacity) 'single-float)) ","
                        "scattercolor"  (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i scatter-color) "'") ","
                        "linecolor"     (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i line-color) "'") ","
                        "strokefill"    (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i stroke-fill) "'") ","
                        "plotcurve"      (write-to-string i) "= d3.curve"
                                            (nth i interpolation) ";"))))
        javascript-string))

;;FUNCTION THAT CREATES D3JS PATHS TO ADD TO PLOT
(defun generate-paths (n)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for generating a string for creating d3js paths
    of the form:

    //THIS IS JAVASCRIPT CODE
    // Creating path using data in pathinfo and path data
    // generator
    if (line){

        // Specify the function for generating path data
        var myline = d3.line()
            .x(function(d){return Xscale(d);})
            .y(function(d,k){return Yscale(ydata[k]);})
            .curve(plotcurve);

        // Append the path to svg according to characteristics
        inner.append('path')
            .attr('d', myline(xdata))
            .style('strokewidth', line-width)
            .style('stroke', linecolor)
            .style('opacity',lineopacity)
            .style('fill', strokefill);
    }
    //END OF JAVASCRIPT CODE

    where xdata, ydata, linewidth, linecolor, lineopacity,
    strokefill and plotcurve are as specified by
    instantiate-javascript

    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - n: Number of paths created in instantiate-javascript that
         are necessary to generate the complete plot (a path
         per list element of instantiate-javascript's x).


    OUTPUT:
    --------------------------------------------------------
    - javascript-string: A string with d3js instructions for
    creating paths between points in plot.

    EXAMPLES:
    --------------------------------------------------------
    ;Create javascript for 3 plot paths:
    (generate-paths 3)

    ;Floats are rounded up
    (generate-paths 2.2)

    --------------------------------------------------------

    "
    (let ((javascript-string "") (i-character ""))
        (setf n (ceiling n)) ;Force number of plots to be an integer
        (loop for i from 0 to (1- n)
            do (progn
                (setf i-character (write-to-string i))
                (setf javascript-string
                    (concatenate 'string javascript-string
                        "
                        // Creating path using data in pathinfo and path data generator
                        if (line" i-character "){

                            // Specify the function for generating path data
                            var myline = d3.line()
                                .x(function(d){return Xscale(d);})
                                .y(function(d,k){return Yscale(ydata" i-character "[k]);})
                                .curve(plotcurve" i-character ");

                            inner.append('path')
                                .attr('d', myline(xdata" i-character "))
                                .style('strokewidth', linewidth" i-character ")
                                .style('stroke', linecolor" i-character ")
                                .style('opacity',lineopacity" i-character ")
                                .style('fill', strokefill" i-character");
                        }"))))
        javascript-string))

;;FUNCTION THAT CREATES D3JS POINTS TO ADD TO PLOT
(defun generate-points (n)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for generating a string for adding points
    (circles) to the d3js plot in the form:

    //THIS IS JAVASCRIPT CODE
    //Creating the scatter
    if (scatter){
        inner.selectAll('scatter-dots')
            .data(ydata)   // using the values in the ydata array
            .enter().append('svg:circle') // create a new circle for each value
            .attr('cy', function(d){return Yscale(d);}) // translate y
            .attr('cx', function(d,k){return Xscale(xdata[k]);})
            .attr('r', radii) // radius of circle
            .style('opacity', scatteropacity)
            .style('fill', scattercolor);
    }
    //END OF JAVASCRIPT CODE

    where xdata, ydata, radii, scattercolor, scatteropacity,
    are as specified by instantiate-javascript.

    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - n: Number of paths created in instantiate-javascript that
         are necessary to generate the complete plot (a
         scatterplot per list element of
         instantiate-javascript's x).


    OUTPUT:
    --------------------------------------------------------
    - javascript-string: A string with d3js instructions for
    creating point scatters for each plot.

    EXAMPLES:
    --------------------------------------------------------
    ;Create javascript for 3 plot scatters:
    (generate-points 3)

    ;Floats are rounded up
    (generate-points 2.2)

    --------------------------------------------------------

    "
    (let ((javascript-string "") (i-character ""))
        (setf n (ceiling n)) ;Force number of plots to be an integer
        (loop for i from 0 to (1- n)
            do (progn
                (setf i-character (write-to-string i))
                (setf javascript-string
                    (concatenate 'string javascript-string
                        "
                        //Creating the scatter
                        if (scatter" i-character "){
                            inner.selectAll('scatter-dots')
                                .data(ydata" i-character ")   // using the values in the ydata array
                                .enter().append('svg:circle') // create a new circle for each value
                                .attr('cy', function(d){return Yscale(d);}) // translate y
                                .attr('cx', function(d,k){return Xscale(xdata" i-character "[k]);})
                                .attr('r', radii" i-character ") // radius of circle
                                .style('opacity', scatteropacity" i-character ")
                                .style('fill', scattercolor" i-character ");
                        }
                        "))))
        javascript-string))

;;FUNCTION THAT ADDS ANNOTATIONS TO A PLOT
(defun generate-annotations (annotations &key (annotations-color NIL) (annotations-font-size NIL))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for generating a string for adding annotations
    (text) to the d3js plot in the form:

    //THIS IS JAVASCRIPT CODE
    //Create annotation and append to svg
    var annotate = inner.append('text')
        .text(ANNOTATION)
        .attr('fill', COLOR))
        .style('font-size', FONT_SIZE)
        .style('font-family', 'sans-serif')
        .attr('text-anchor', 'middle')
        .attr('x', Xscale(X_POSITION))
        .attr('y', Yscale(Y_POSITION));
    //END OF JAVASCRIPT CODE

    where
    ANNOTATION corresponds to the first element of the ith list (element of) on annotations
    X_POSITION corresponds to the second element of each list on annotations
    Y_POSITION corresponds to the third element of each list on annotations
    COLOR      corresponds to the ith element of each list on annotations-color
    FONT_SIZE  corresponds to the ith element of each list on annotations-font-size


    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - annotations: a list of lists. Each element of annotations
                   must have text to annotate, an x position
                   and a y position to add annotations to
                   the plot. Thus annotations must look as:
                   (list (list 'someText1' xpos1 ypos1)
                         (list 'someText2' xpos2 ypos2))
    - annotations-color: a list of colors for each annotation
    - annotations-font-size: a list of font-sizes for each
                             annotation


    OUTPUT:
    --------------------------------------------------------
    - javascript-string: A string with d3js instructions for
    adding annotations to each element of the plot.

    EXAMPLES:
    --------------------------------------------------------
    ;Create javascript for 2 annotations:
    (generate-annotations (list (list 'hello' 1 1) (list 'world' 5 1)))

    --------------------------------------------------------

    "

    ;Make annotations a list of lists
    (unless (listp (first annotations)) (setf annotations (list annotations)))

    ;Check that colors are assigned and are of correct size
    (when (null annotations-color)
        (setf annotations-color (make-list (length annotations) :initial-element "black")))
    (setf annotations-color (increase-list-length annotations-color (length annotations)))

    ;Check that font-size are assigned and are of correct size
    (when (null annotations-font-size)
        (setf annotations-font-size (make-list (length annotations) :initial-element 12)))
    (setf annotations-font-size (increase-list-length annotations-font-size (length annotations)))

    ;;Create the javascript code
    (let ((javascript-string ""))
        (unless (null annotations)
            (loop for i from 0 to (1- (length annotations))
                do (progn
                    (setf javascript-string (concatenate 'string javascript-string
                        "
                        //Create annotation and append to svg
                        var annotate" (write-to-string i) " = inner.append('text')
                            .text('"(first (nth i annotations)) "')
                            .attr('fill'," (concatenate 'string "'"
                                            (nth i annotations-color) "'") ")
                            .style('font-size'," (write-to-string
                                            (coerce (nth i annotations-font-size) 'single-float)) ")
                            .style('font-family', 'sans-serif')
                            .attr('text-anchor', 'middle')
                            .attr('x', Xscale("  (write-to-string
                                            (coerce (second (nth i annotations)) 'single-float)) "))
                            .attr('y', Yscale("  (write-to-string
                                            (coerce (third (nth i annotations)) 'single-float)) "));
                        "
                    )))))
        javascript-string))
