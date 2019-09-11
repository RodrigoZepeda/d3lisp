;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: D3 -*-
(in-package #:d3)

;;FUNCTION THAT TRANSFORMS LISP LIST TO JAVASCRIPT ARRAY
(defun to-javascript-array (x)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for transforming a numerical LISP list to a
    Javascript array
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x: A LISP numerical list

    OUTPUT:
    --------------------------------------------------------
    - javascript-array: A Javascript array containing the
    elements of x

    EXAMPLES:
    --------------------------------------------------------
    ;Create an array with numbers 1, 2 and 3
    (to-javascript-array (list 1 2 3))

    ;List elements must be numeric
    (to-javascript-array (list 'a' 2 3))
    --------------------------------------------------------

    "
    (when (and (listp x) (not (null x)))
        (let ((javascript-array "["))
            (dolist (xelem (butlast x) javascript-array)
                (setf javascript-array
                  (concatenate 'string javascript-array
                    (write-to-string (coerce xelem 'single-float)) ",")))
            (setf javascript-array
                (concatenate 'string javascript-array (write-to-string
                  (coerce (car (last x)) 'single-float)) "]")))))

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
    """
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

    """
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
                        "line-width"     (write-to-string i) "=" (write-to-string
                                            (coerce (nth i line-width) 'single-float)) ","
                        "line-opacity"   (write-to-string i) "=" (write-to-string
                                            (coerce (nth i line-opacity) 'single-float)) ","
                        "scatter-opacity"(write-to-string i) "=" (write-to-string
                                            (coerce (nth i scatter-opacity) 'single-float)) ","
                        "scatter-color"  (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i scatter-color) "'") ","
                        "line-color"     (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i line-color) "'") ","
                        "stroke-fill"    (write-to-string i) "=" (concatenate 'string "'"
                                            (nth i stroke-fill) "'") ","
                        "plotcurve"      (write-to-string i) "= d3.curve"
                                            (nth i interpolation) ";"))))
        javascript-string))

(defun generate-lines (n)
    (let ((javascript-string "") (ichar ""))
        (loop for i from 0 to (1- n)
            do (progn
                (setf ichar (write-to-string i))
                (setf javascript-string
                    (concatenate 'string javascript-string
                        "
                        // Creating path using data in pathinfo and path data generator
                        if (line" ichar "){

                            // Specify the function for generating path data
                            var myline = d3.line()
                                .x(function(d){return Xscale(d);})
                                .y(function(d,i){return Yscale(ydata" ichar "[i]);})
                                .curve(plotcurve" ichar ");

                            inner.append('path')
                                .attr('d', myline(xdata" ichar "))
                                .style('stroke-width', line-width" ichar ")
                                .style('stroke', line-color" ichar ")
                                .style('opacity',line-opacity" ichar ")
                                .style('fill', stroke-fill" ichar");
                        }
                        "
                    )
                )
            )
        )
        javascript-string
    )
)

(defun generate-scatters (n)
    (let ((javascript-string "") (ichar ""))
        (loop for i from 0 to (1- n)
            do (progn
                (setf ichar (write-to-string i))
                (setf javascript-string
                    (concatenate 'string javascript-string
                        "
                        if (scatter" ichar "){
                                        inner.selectAll('scatter-dots')
                                            .data(ydata" ichar ")  // using the values in the ydata0 array
                                            .enter().append('svg:circle')  // create a new circle for each value
                                            .attr('cy', function (d) { return Yscale(d); } ) // translate y value to a pixel
                                            .attr('cx', function (d,i) { return Xscale(xdata" ichar "[i]); } ) // translate x value
                                            .attr('r', radii" ichar ") // radius of circle
                                            .style('opacity',scatter-opacity" ichar ")
                                            .style('fill', scatter-color" ichar ");
                                    }
                        "
                    )
                )
            )
        )
        javascript-string
    )
)

(defun generate-annotations (annotations &key (annotations-color NIL) (annotations-fontsize NIL))

    (if (not (listp (first annotations)))
        (setf annotations (list annotations))
    )
    (if (null annotations-color)
        (setf annotations-color (make-list (length annotations) :initial-element "black"))
    )
    (if (null annotations-fontsize)
        (setf annotations-fontsize (make-list (length annotations) :initial-element 12))
    )

    (let ((javascript-string ""))
        (if (not (null annotations))
            (loop for i from 0 to (1- (length annotations))
                do (progn
                    (setf javascript-string (concatenate 'string javascript-string
                        "var annotate" (write-to-string i) " = inner.append('text')
                                            .text('"(first (nth i annotations)) "')
                                            .attr('fill'," (concatenate 'string "'" (nth i annotations-color) "'") ")
                                            .style('font-size'," (write-to-string (coerce  (nth i annotations-fontsize) 'single-float)) ")
                                            .style('font-family', 'sans-serif')
                                            .attr('text-anchor', 'middle')
                                            .attr('x', Xscale("  (write-to-string (coerce (second (nth i annotations)) 'single-float)) "))
                                            .attr('y', Yscale("  (write-to-string (coerce (third (nth i annotations)) 'single-float)) "));
                        "
                    ))
                )
            )
        )
        javascript-string
    )
    )
