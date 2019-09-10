(defun htmlcode (x y  &key (xlab "My x label") (ylab "My y label") (title "My Title") (size 7)
                            (xmin NIL) (ymin NIL) (xmax NIL) (ymax NIL) (scattercolor "green")
                            (linecolor "purple") (linewidth 3) (strokefill "none") (line T) (scatter T)
                            (interpolation "Linear") (showXaxis T) (showYaxis T)
                            (axisX_label_color "black") (axisY_label_color "black")
                            (axisX_tick_color "black") (axisY_tick_color "black") (axisX_color "black")
                            (axisY_color "black") (lineopacity 1) (scatteropacity 1)
                            (yaxispos NIL) (xaxispos NIL)
                            (plotheight "default") (plotwidth "default")
                            (outercolor "none") (innercolor "none") (annotations NIL)
                            (annotations-color "black") (annotations-fontsize 12)
                            (title-fontsize 18) (title-color "black")
                            (margin (list 10 10 10 10)) (padding (list 30 30 60 60))
                            (squareplot NIL) (save NIL) (svgname "Myplot")
                            (plotnum 1)
                            )

    (if (not (listp (first x)))
        (setf x (list x))
    )

    (if (not (listp (first y)))
        (setf y (list y))
    )

    (if (null xmin)
        (setf xmin (apply #'min  (apply #'mapcar #'min x)))
    )

    (if (null ymin)
        (setf ymin (apply #'min (apply #'mapcar #'min y)))
    )

    (if (null xmax)
        (setf xmax (apply #'max (apply #'mapcar #'max x)))
    )

    (if (null ymax)
        (setf ymax (apply #'max (apply #'mapcar #'max y)))
    )

    (if (null yaxispos)
        (if (or (< xmax 0) (> xmin 0))
            (setf yaxispos xmin)
            (setf yaxispos 0)
        )
    )

    (if (null xaxispos)
        (if (or (< ymax 0) (> ymin 0))
            (setf xaxispos ymin)
            (setf xaxispos 0)
        )
    )

    (if (not (stringp plotheight)) (setf plotheight (write-to-string (coerce plotheight 'single-float))))
    (if (not (stringp plotwidth))  (setf plotwidth (write-to-string (coerce plotwidth 'single-float))))

    (if (string= plotheight "default")
        (setf plotheight "0.9*Math.max(document.documentElement['clientHeight'], document.body['scrollHeight'], document.documentElement['scrollHeight'], document.body['offsetHeight'], document.documentElement['offsetHeight'])")
        (setf plotheight (concatenate 'string "'" plotheight "'"))
    )

    (if (string= plotwidth "default")
        (setf plotwidth "0.9*Math.max(document.documentElement['clientWidth'], document.body['scrollWidth'], document.documentElement['scrollWidth'], document.body['offsetWidth'], document.documentElement['offsetWidth'])")
        (setf plotwidth (concatenate 'string "'" plotwidth "'"))
    )

    ;Check lengths of line-specific variables
    (setf scatter        (checklengths scatter           (length x)))
    (setf line           (checklengths line              (length x)))
    (setf size           (checklengths size              (length x)))
    (setf linewidth      (checklengths linewidth         (length x)))
    (setf lineopacity    (checklengths lineopacity       (length x)))
    (setf scatteropacity (checklengths scatteropacity    (length x)))
    (setf scattercolor   (checklengths scattercolor      (length x)))
    (setf linecolor      (checklengths linecolor         (length x)))
    (setf strokefill     (checklengths strokefill        (length x)))
    (setf interpolation  (checklengths interpolation     (length x)))
    (setf interpolation  (checklengths interpolation     (length x)))
    (if (and (listp annotations) (not (null annotations)))
        (progn
            (setf annotations-color     (checklengths annotations-color     (length annotations)))
            (setf annotations-fontsize  (checklengths annotations-fontsize  (length annotations)))
        )
    )

    ;;Transform line and scatter cases to string
    (setf line (mapcar (lambda (l) (if l "true" "false")) line))
    (setf scatter (mapcar (lambda (l) (if l "true" "false")) scatter))

    ;Signal length error
    (if (not (= (length x) (length y)))
        (error "Y and x don't have same length")
    )

    (if showXaxis
        (setf showXaxis "true")
        (setf showXaxis "false")
    )

    (if showYaxis
        (setf showYaxis "true")
        (setf showYaxis "false")
    )

    (if squareplot
        (setf squareplot "true")
        (setf squareplot "false")
    )

    (if save
        (setf save "true")
        (setf save "false")
    )

    (concatenate 'string
        "
        <!DOCTYPE html>
        <html lang='en'>
            <head>
                    <title>Plot " (write-to-string plotnum) "</title>

                    <meta charset = 'utf-8'>
                    <meta name='viewport' content='width=device-width, initial-scale=1'>
                    <meta http-equiv='X-UA-Compatible' content='IE=edge'>
                    <meta name='description' content='d3lisp plot'>
                    <meta name='author' content='d3lisp'>

                    <!-- Favicon ico to include logo -->
                    <link rel='shortcut icon' href='/webpage/favicon.ico' type='image/x-icon'>
                    <link rel='icon' href='/webpage/favicon.ico' type='image/x-icon'>

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
                                    (generatevars x y :scatter scatter :line line :size size
                                        :linewidth linewidth :lineopacity lineopacity
                                        :scatteropacity scatteropacity :scattercolor scattercolor
                                        :linecolor linecolor :strokefill strokefill
                                        :interpolation interpolation)

"/*---------------------------------END OF VARIABLES FOR LISP LOOP*/
"
                                    "var xlab       = "  (concatenate 'string "'" xlab "'")  ";"
                                    "var ylab       = "  (concatenate 'string "'" ylab "'")  ";"
                                    "var title      = "  (concatenate 'string "'" title "'") ";"
                                    "var filename   = "  (concatenate 'string "'" svgname "." "svg" "'") ";"
                                    "var xmin       = "  (write-to-string (coerce xmin 'single-float))  ";"
                                    "var xmax       = "  (write-to-string (coerce xmax 'single-float))  ";"
                                    "var ymin       = "  (write-to-string (coerce ymin 'single-float))  ";"
                                    "var ymax       = "  (write-to-string (coerce ymax 'single-float))  ";"
                                    "var xaxispos   = "  (write-to-string (coerce xaxispos 'single-float)) ";"
                                    "var yaxispos   = "  (write-to-string (coerce yaxispos 'single-float)) ";"
                                    "var showXaxis  = "  showXaxis ";"
                                    "var showYaxis  = "  showYaxis ";"
                                    "var save       = "  save ";"
                                    "var squareplot = "  squareplot ";"
                                    "var axisX_color        = " (concatenate 'string "'" axisX_color "'") ";"
                                    "var axisY_color        = " (concatenate 'string "'" axisY_color "'") ";"
                                    "var axisX_label_color  = " (concatenate 'string "'" axisX_label_color "'") ";"
                                    "var axisY_label_color  = " (concatenate 'string "'" axisY_label_color "'") ";"
                                    "var axisX_tick_color   = " (concatenate 'string "'" axisX_tick_color "'") ";"
                                    "var axisY_tick_color   = " (concatenate 'string "'" axisY_tick_color "'") ";"
                                    "var outercolor         = " (concatenate 'string "'" outercolor "'") ";"
                                    "var innercolor         = " (concatenate 'string "'" innercolor "'") ";"
                                    "var titlefontsize      = " (write-to-string (coerce title-fontsize 'single-float)) ";"
                                    "var titlecolor         = " "'" title-color "'" ";"
                                    "var outerHeight = " plotheight ";"
                                    "var outerWidth  = " plotwidth ";"
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
                                               .attr('fill', outercolor)
                                               .attr('width', innerWidth)
                                               .attr('height', innerHeight);

                                        var inner = outer.append('g')
                                                        .attr('transform', 'translate(' + padding.left + ',' + padding.top + ')');

                                        //Rectangles for debugging
                                        inner.append('rect')
                                            .attr('fill', innercolor)
                                            .attr('width', width)
                                            .attr('height', height);

                                        //X.axis
                                        Xscale = d3.scaleLinear()
                                                    .domain([xmin, xmax])
                                                    .range([0, width]);

                                        //Y-axis
                                        Yscale = d3.scaleLinear()
                                                    .domain([ymin, ymax])
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
                                        (generate-lines (length x))
                                        (generate-scatters (length x))
                                        (generate-annotations annotations :annotations-color annotations-color
                                            :annotations-fontsize annotations-fontsize)
"/*--------------------------------------END OF PART THAT NEEDS TO BE IN LISP LOOP*/

                                        if (showXaxis){
                                            axisX  = inner.append('g')
                                                            .attr('transform', 'translate(' + 0 + ',' + Yscale(xaxispos) + ')')
                                                            .style('stroke', axisX_tick_color)
                                                            .call(d3.axisBottom(Xscale));

                                            axisX.selectAll('line').style('stroke', axisX_tick_color);
                                            axisX.selectAll('path').style('stroke', axisX_color);

                                            //Create X axis label
                                            outer.append('text')
                                                .attr('x', innerWidth / 2 )
                                                .attr('y',  innerHeight - padding.top/2)
                                                .style('text-anchor', 'middle')
                                                .style('font-family', 'sans-serif')
                                                .style('fill', axisX_label_color)
                                                .text(xlab);

                                        }

                                        if (showYaxis){
                                            axisY = inner.append('g')
                                                        .attr('transform', 'translate(' + Xscale(yaxispos) + ',' + 0 + ')')
                                                        .style('stroke', axisX_tick_color)
                                                        .call(d3.axisLeft(Yscale));

                                            axisY.selectAll('line').style('stroke', axisY_tick_color);
                                            axisY.selectAll('path').style('stroke', axisY_color);

                                            //Create Y axis label
                                            outer.append('text')
                                                .attr('y', 0)
                                                .attr('x', 0)
                                                .attr('transform', 'translate(' + 0 + ',' + innerHeight/2 + ') rotate(-90)')
                                                .attr('dy', '1em')
                                                .style('text-anchor', 'middle')
                                                .style('font-family', 'sans-serif')
                                                .style('fill', axisY_label_color)
                                                .text(ylab);
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
            </body>
        </html>"
    )
)
