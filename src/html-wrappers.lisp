;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;;FUNCTION FOR GENERATING HTML CODE FOR THE PLOT
(defun generate-html-page (x y  &key (x-label "My x label")(y-label "My y label")
                            (title "My Title")(size 7)(x-minimum NIL)(y-minimum NIL)
                            (x-maximum NIL)(y-maximum NIL)(scatter-color "green")
                            (line-color "purple")(line-width 3)(stroke-fill "none")(line T)
                            (scatter T)(interpolation "Linear")(show-x-axis T)(show-y-axis T)
                            (axis-x-label-color "black")(axis-y-label-color "black")
                            (axis-x-tick-color "black")(axis-y-tick-color "black")
                            (axis-x-color "black")(axis-y-color "black")(line-opacity 1)
                            (scatter-opacity 1)(y-axis-position NIL)(x-axis-position NIL)
                            (plot-height "default")(plot-width "default")
                            (outer-background-color "none")(inner-background-color "none")
                            (annotations NIL)(annotations-color "black")(annotations-font-size 12)
                            (title-font-size 18)(title-color "black")
                            (margin (list 10 10 10 10))(padding (list 30 30 60 60))
                            (square-plot NIL)(save NIL)(svg-name "Myplot")(plot-number 1))

    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for generating a string with html code for a plot
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
    A string with html code plotting x vs y

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
    ;Plot a line:
    (generate-html-page (list 1 2 3) (list 1 2 3))
    --------------------------------------------------------

    "

    ;Check the x and y coordinates form a list of lists
    (unless (listp (first x)) (setf x (list x)))
    (unless (listp (first y)) (setf y (list y)))

    ;Signal length error
    (unless (= (length x) (length y)) (error "Y and x don't have same length"))

    ;Set the min and max of the axis
    (when (null x-minimum) (setf x-minimum (apply #'min (apply #'mapcar #'min x))))
    (when (null y-minimum) (setf y-minimum (apply #'min (apply #'mapcar #'min y))))
    (when (null x-maximum) (setf x-maximum (apply #'max (apply #'mapcar #'max x))))
    (when (null y-maximum) (setf y-maximum (apply #'max (apply #'mapcar #'max y))))

    ;Center the y-axis at 0 or at the minimum
    (when (null y-axis-position)
        (if (or (< x-maximum 0) (> x-minimum 0))
            (setf y-axis-position x-minimum)
            (setf y-axis-position 0)))

    ;Center the x-axis at 0 or at the maximum
    (when (null x-axis-position)
        (if (or (< y-maximum 0) (> y-minimum 0))
            (setf x-axis-position y-minimum)
            (setf x-axis-position 0)))

    ;Verify plot height is given
    (unless (stringp plot-height)
        (setf plot-height (write-to-string (coerce plot-height 'single-float))))
    (unless (stringp plot-width)
        (setf plot-width  (write-to-string (coerce plot-width  'single-float))))

    ;For the plot height set the greatest height available for the html
    (if (string= plot-height "default")
        (setf plot-height
          (concatenate 'string
            "0.9*Math.max("
              "document.documentElement['clientHeight'],"
              "document.body['scrollHeight'],"
              "document.documentElement['scrollHeight'],"
              "document.body['offsetHeight'],"
              "document.documentElement['offsetHeight'])"))
        (setf plot-height (concatenate 'string "'" plot-height "'")))

    ;For the plot height set the greatest height available for the html
    (if (string= plot-width "default")
        (setf plot-width
          (concatenate 'string
           "0.9*Math.max("
             "document.documentElement['clientWidth'],"
             "document.body['scrollWidth'],"
             "document.documentElement['scrollWidth'],"
             "document.body['offsetWidth'],"
             "document.documentElement['offsetWidth'])"))
        (setf plot-width (concatenate 'string "'" plot-width "'")))


    ;Check lengths of line-specific variables if the length is smaller,
    ;assign the last element of the list to all
    (setf scatter         (increase-list-length scatter         (length x)))
    (setf line            (increase-list-length line            (length x)))
    (setf size            (increase-list-length size            (length x)))
    (setf line-width      (increase-list-length line-width      (length x)))
    (setf line-opacity    (increase-list-length line-opacity    (length x)))
    (setf scatter-opacity (increase-list-length scatter-opacity (length x)))
    (setf scatter-color   (increase-list-length scatter-color   (length x)))
    (setf line-color      (increase-list-length line-color      (length x)))
    (setf stroke-fill     (increase-list-length stroke-fill     (length x)))
    (setf interpolation   (increase-list-length interpolation   (length x)))


    ;If there are given annotations set the font size and colors in the same manner
    (when (and (listp annotations) (not (null annotations)))
      (progn
        (setf annotations-color
          (increase-list-length annotations-color     (length annotations)))
        (setf annotations-font-size
          (increase-list-length annotations-font-size  (length annotations)))))

    ;;Transform line and scatter cases to string
    (setf line    (mapcar (lambda (l) (if l "true" "false")) line))
    (setf scatter (mapcar (lambda (l) (if l "true" "false")) scatter))

    (if show-x-axis
        (setf show-x-axis "true")
        (setf show-x-axis "false"))

    (if show-y-axis
        (setf show-y-axis "true")
        (setf show-y-axis "false"))

    (if square-plot
        (setf square-plot "true")
        (setf square-plot "false"))

    (if save
        (setf save "true")
        (setf save "false"))

    (concatenate 'string
        "
        <!DOCTYPE html>
        <html lang='en'>
            <head>
                    <title>Plot " (write-to-string plot-number) "</title>

                    <meta charset = 'utf-8'>
                    <meta name='viewport' content='width=device-width, initial-scale=1'>
                    <meta http-equiv='X-UA-Compatible' content='IE=edge'>
                    <meta name='description' content='d3lisp plot'>
                    <meta name='author' content='d3lisp'>

                    <!-- Favicon ico to include logo -->
                    <link rel='shortcut icon' href='favicon.ico' type='image/x-icon'>
                    <link rel='icon' href='favicon.ico' type='image/x-icon'>

                    <!--Nice bootstrap format (could be simplified to improve loading)-->
                    <link href='https://bootswatch.com/3/cerulean/bootstrap.css' rel='stylesheet' media = 'all'>

                    <!-- Inclusion of d3js for plotting-->
                    <script src='https://d3js.org/d3.v5.min.js'></script>

                    <!-- Saving libraries -->
                    <script src='https://cdn.rawgit.com/eligrey/canvas-toBlob.js/master/canvas-toBlob.js'></script>
	                  <script src='https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js'></script>
                    <script src='https://cdn.rawgit.com/eligrey/Blob.js/master/Blob.js'></script>
            </head>
            <body>
                <div class = 'panel panel-default'>
                        <div class='panel-heading'><button id='generate' class='btn btn-primary btn-sm'>Save</button></div>
                        <div class='panel-body'>
                            <div class = 'row'>
                                <div id = 'myfigure' style = 'text-align:center;'></div>

                                <!-- This is the part that lisp changes -->"
                                "<script>"
"/*---------------------------------VARIABLES FOR LISP LOOP*/
"
                                    (instantiate-javascript x y :scatter scatter :line line
                                        :size size :line-width line-width
                                        :line-opacity line-opacity :scatter-opacity scatter-opacity
                                        :scatter-color scatter-color :line-color line-color
                                        :stroke-fill stroke-fill :interpolation interpolation)

"/*---------------------------------END OF VARIABLES FOR LISP LOOP*/
"
                                    "var xlabel       = "  (concatenate 'string "'" x-label "'")  ";"
                                    "var ylabel       = "  (concatenate 'string "'" y-label "'")  ";"
                                    "var title        = "  (concatenate 'string "'" title "'") ";"
                                    "var filename     = "  (concatenate 'string "'" svg-name "." "svg" "'") ";"
                                    "var xminimum       = "  (write-to-string (coerce x-minimum 'single-float))  ";"
                                    "var xmaximum       = "  (write-to-string (coerce x-maximum 'single-float))  ";"
                                    "var yminimum       = "  (write-to-string (coerce y-minimum 'single-float))  ";"
                                    "var ymaximum       = "  (write-to-string (coerce y-maximum 'single-float))  ";"
                                    "var xaxisposition   = "  (write-to-string (coerce x-axis-position 'single-float)) ";"
                                    "var yaxisposition   = "  (write-to-string (coerce y-axis-position 'single-float)) ";"
                                    "var showxaxis  = "  show-x-axis ";"
                                    "var showyaxis  = "  show-y-axis ";"
                                    "var save       = "  save ";"
                                    "var squareplot = "  square-plot ";"
                                    "var axisxcolor        = " (concatenate 'string "'" axis-x-color "'") ";"
                                    "var axisycolor        = " (concatenate 'string "'" axis-y-color "'") ";"
                                    "var axisxlabelcolor  = " (concatenate 'string "'" axis-x-label-color "'") ";"
                                    "var axisylabelcolor  = " (concatenate 'string "'" axis-y-label-color "'") ";"
                                    "var axisxtickcolor   = " (concatenate 'string "'" axis-x-tick-color "'") ";"
                                    "var axisytickcolor   = " (concatenate 'string "'" axis-y-tick-color "'") ";"
                                    "var outerbackgroundcolor         = " (concatenate 'string "'" outer-background-color "'") ";"
                                    "var innerbackgroundcolor         = " (concatenate 'string "'" inner-background-color "'") ";"
                                    "var titlefontsize      = " (write-to-string (coerce title-font-size 'single-float)) ";"
                                    "var titlecolor         = " "'" title-color "'" ";"
                                    "var outerHeight = " plot-height ";"
                                    "var outerWidth  = " plot-width ";"
                                    "var margin  = {top:"    (write-to-string (coerce (nth 0 margin) 'single-float)) ",
                                                    right:"  (write-to-string (coerce (nth 1 margin) 'single-float)) ",
                                                    bottom:" (write-to-string (coerce (nth 2 margin) 'single-float)) ",
                                                    left:"   (write-to-string (coerce (nth 3 margin) 'single-float)) "};"
                                    "var padding = {top:"    (write-to-string (coerce (nth 0 padding) 'single-float)) ",
                                                    right:"  (write-to-string (coerce (nth 1 padding) 'single-float)) ",
                                                    bottom:" (write-to-string (coerce (nth 2 padding) 'single-float)) ",
                                                    left:"   (write-to-string (coerce (nth 3 padding) 'single-float)) "};"
                                    "var axisxcolor        = " (concatenate 'string "'" axis-x-color "'") ";"
                                    "var axisycolor        = " (concatenate 'string "'" axis-y-color "'") ";"
                                    "var axisxlabelcolor  = " (concatenate 'string "'" axis-x-label-color "'") ";"
                                    "var axisylabelcolor  = " (concatenate 'string "'" axis-y-label-color "'") ";"
                                    "var axisxtickcolor   = " (concatenate 'string "'" axis-x-tick-color "'") ";"
                                    "var axisytickcolor   = " (concatenate 'string "'" axis-y-tick-color "'") ";"
                                    "var outerbackgroundcolor         = " (concatenate 'string "'" outer-background-color "'") ";"
                                    "var innerbackgroundcolor         = " (concatenate 'string "'" inner-background-color "'") ";"
                                    "var titlefontsize      = " (write-to-string (coerce title-font-size 'single-float)) ";"
                                    "var titlecolor         = " "'" title-color "'" ";"
                                    "var outerHeight = " plot-height ";"
                                    "var outerWidth  = " plot-width ";"
                                    "var margin  = {top:"    (write-to-string (coerce (nth 0 margin) 'single-float)) ",
                                                    right:"  (write-to-string (coerce (nth 1 margin) 'single-float)) ",
                                                    bottom:" (write-to-string (coerce (nth 2 margin) 'single-float)) ",
                                                    left:"   (write-to-string (coerce (nth 3 margin) 'single-float)) "};"
                                    "var padding = {top:"    (write-to-string (coerce (nth 0 padding) 'single-float)) ",
                                                    right:"  (write-to-string (coerce (nth 1 padding) 'single-float)) ",
                                                    bottom:" (write-to-string (coerce (nth 2 padding) 'single-float)) ",
                                                    left:"   (write-to-string (coerce (nth 3 padding) 'single-float)) "};"
                                "</script>"
    "                           <!-- D3JS script for plotting-->
                                <script>
                                        if (squareplot){
                                            outerHeight  = Math.min(outerHeight, outerWidth);
                                            outerWidth   = Math.min(outerHeight, outerWidth);
                                        }

                                        var innerWidth   = outerWidth  - margin.left  - margin.right,
                                            innerHeight  = outerHeight - margin.top   - margin.bottom,
                                            width        = innerWidth  - padding.left - padding.right,
                                            height       = innerHeight - padding.top  - padding.bottom,
                                            background   = false;

                                        //Creation of canvas
                                        var outer = d3.select('#myfigure').append('svg')
                                                        .attr('width', outerWidth)
                                                        .attr('height', outerHeight);

                                        //Rectangles for debugging
                                        outer.append('rect')
                                               .attr('fill', outerbackgroundcolor)
                                               .attr('width', innerWidth)
                                               .attr('height', innerHeight);

                                        var inner = outer.append('g')
                                                        .attr('transform', 'translate(' + padding.left + ',' + padding.top + ')');

                                        //Rectangles for debugging
                                        inner.append('rect')
                                            .attr('fill', innerbackgroundcolor)
                                            .attr('width', width)
                                            .attr('height', height);

                                        //X.axis
                                        Xscale = d3.scaleLinear()
                                                    .domain([xminimum, xmaximum])
                                                    .range([0, width]);

                                        //Y-axis
                                        Yscale = d3.scaleLinear()
                                                    .domain([yminimum, ymaximum])
                                                    .range([height, 0]);

                                        //Create plot title
                                        inner.append('text')
                                                .attr('x', (width / 2))
                                                .attr('y', 0 - (padding.top / 2))
                                                .attr('text-anchor', 'middle')
                                                .style('font-size', titlefontsize)
                                                .style('font-family', 'sans-serif')
                                                .style('font-weight', 'bold')
                                                .attr('fill', titlecolor)
                                                .text(title);

/*--------------------------------------PART THAT NEEDS TO BE IN LISP LOOP*/"

                                        (generate-paths  (length x))

                                        (generate-points (length x))

                                        (generate-annotations annotations
                                          :annotations-color annotations-color
                                          :annotations-font-size annotations-font-size)

"/*--------------------------------------END OF PART THAT NEEDS TO BE IN LISP LOOP*/

                                        if (showxaxis){
                                            axisX  = inner.append('g')
                                                            .attr('transform', 'translate(' + 0 + ',' + Yscale(xaxisposition) + ')')
                                                            .style('stroke', axisxtickcolor)
                                                            .call(d3.axisBottom(Xscale));

                                            axisX.selectAll('line').style('stroke', axisxtickcolor);
                                            axisX.selectAll('path').style('stroke', axisxcolor);

                                            //Create X axis label
                                            outer.append('text')
                                                .attr('x', innerWidth / 2 )
                                                .attr('y',  innerHeight - padding.top/2)
                                                .style('text-anchor', 'middle')
                                                .style('font-family', 'sans-serif')
                                                .style('fill', axisxlabelcolor)
                                                .text(xlabel);

                                        }

                                        if (show-y-axis){
                                            axisY = inner.append('g')
                                                        .attr('transform', 'translate(' + Xscale(yaxisposition) + ',' + 0 + ')')
                                                        .style('stroke', axisxtickcolor)
                                                        .call(d3.axisLeft(Yscale));

                                            axisY.selectAll('line').style('stroke', axisytickcolor);
                                            axisY.selectAll('path').style('stroke', axisycolor);

                                            //Create Y axis label
                                            outer.append('text')
                                                .attr('y', 0)
                                                .attr('x', 0)
                                                .attr('transform', 'translate(' + 0 + ',' + innerHeight/2 + ') rotate(-90)')
                                                .attr('dy', '1em')
                                                .style('text-anchor', 'middle')
                                                .style('font-family', 'sans-serif')
                                                .style('fill', axisylabelcolor)
                                                .text(ylabel);
                                        }

                                        d3.selectAll('.tick > text').style('font-weight','lighter');

                                    </script>"
                                    "<!-- SCRIPT FOR SAVING; ADAPTED FROM http://bl.ocks.org/wboykinm/e6e222d71e9b59e8b3053e0c4fe83daf -->
                                    <script>
                                        d3.select('#generate')
                                            .on('click', writeDownloadLink);

                                        function writeDownloadLink(){
                                            try {
                                                var isFileSaverSupported = !!new Blob();
                                            } catch (e) {
                                                alert('blob not supported');
                                            }

                                            var html = d3.select('svg')
                                                .attr('title', 'test2')
                                                .attr('version', 1.1)
                                                .attr('xmlns', 'http://www.w3.org/2000/svg')
                                                .node().parentNode.innerHTML;

                                            var blob = new Blob([html], {type: 'image/svg+xml'});
                                            saveAs(blob, filename);
                                        };
                                        if (save){
                                            window.onload = function(){document.getElementById('generate').click();};
                                        }

                                    </script>
                            </div>
                        </div>
                    </div>
                </body>
            </html>"))
