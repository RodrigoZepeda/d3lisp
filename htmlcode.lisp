(defun htmlcode (x y  &key (xlab "My x label") (ylab "My y label") (title "My Title") (size 7) 
                            (xmin NIL) (ymin NIL) (xmax NIL) (ymax NIL) (scattercolor "green")
                            (linecolor "purple") (linewidth 3) (strokefill "none") (line T) (scatter T)
                            (interpolation "Linear") (showXaxis T) (showYaxis T)
                            (axisX_label_color "black") (axisY_label_color "black")
                            (axisX_tick_color "black") (axisY_tick_color "black") (axisX_color "black")
                            (axisY_color "black") (lineopacity 1) (scatteropacity 1) 
                            (outercolor "none") (innercolor "none")
                            (squareplot NIL) (save NIL) (filename "Myplot") (fileformat "png")
                            )
    "Interpolations: Linear | Step | StepBefore | StepAfter | Basis | Cardinal | MonotoneX | CatmullRom"
    
    (if (null xmin)
        (setf xmin "Math.min.apply(null, xdata) - 1")
        (setf xmin (write-to-string xmin))
    )

    (if (null ymin)
        (setf ymin "Math.min.apply(null, ydata) - 1")
        (setf ymin (write-to-string ymin))
    )

    (if (null xmax)
        (setf xmax "Math.max.apply(null, xdata) + 1")
        (setf xmax (write-to-string xmax))
    )

    (if (null ymax)
        (setf ymax "Math.max.apply(null, ydata) + 1")
        (setf ymax (write-to-string ymax))
    )

    (if line
        (setf line "true")
        (setf line "false")
    )

    (if scatter
        (setf scatter "true")
        (setf scatter "false")
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
                    <meta chsrset = 'utf-8'>
                    <meta name='viewport' content='width=device-width, initial-scale=1'>
                    <meta http-equiv='X-UA-Compatible' content='IE=edge'>
                    <meta name='description' content='Awesome plot'>
                    <meta name='author' content='Rodrigo Zepeda Tello'>

                    <!--Nice bootstrap format (could be simplified to improve loading)-->
                    <link href='https://bootswatch.com/3/cerulean/bootstrap.css' rel='stylesheet' media = 'all'>

                    <!-- Inclusion of d3js for plotting-->
                    <script src='https://d3js.org/d3.v5.min.js'></script>

                    <!-- Saving libraries -->
                    <script src='https://cdn.rawgit.com/eligrey/canvas-toBlob.js/master/canvas-toBlob.js'></script>
	                <script src='https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js'></script>
                    <script src='https://cdn.rawgit.com/eligrey/Blob.js/master/Blob.js'></script>

                    <!-- Mathjax might be interesting to add to plot-->
                    <script type='text/javascript' src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML'></script>
                    <script type='text/x-mathjax-config'>
                        MathJax.Hub.Config({
                            tex2jax: {
                                inlineMath: [['$','$'], ['\\(','\\)']],
                                processEscapes: true,
                            },
                        });
                    </script>
            </head>
            <body>
                <div class = 'panel panel-default'>
                        <div class='panel-heading'><button id='generate' class='btn btn-primary btn-sm'>Save</button></div>
                        <div class='panel-body'>
                            <div class = 'row'>
                                <div class='form-group col-lg-9' id = 'myfigure' style = 'text-align:center;'></div>
                                
                                <!-- This is the part that lisp changes -->"
                                "<script>"
                                    "var xdata = "  (to_javascript_array x) ";"
                                    "var ydata = "  (to_javascript_array y) ";"
                                    "var xlab  = "  (concatenate 'string "'" xlab "'")  ";"
                                    "var ylab  = "  (concatenate 'string "'" ylab "'")  ";"
                                    "var title = "  (concatenate 'string "'" title "'") ";"
                                    "var fileformat = "  (concatenate 'string "'" fileformat "'") ";"
                                    "var filename = " (concatenate 'string "'" filename "." fileformat "'") ";"
                                    "var xmin  = "  xmin ";"
                                    "var xmax  = "  xmax ";"
                                    "var ymin  = "  ymin ";"
                                    "var ymax  = "  ymax ";"
                                    "var showXaxis  = " showXaxis ";"
                                    "var showYaxis  = " showYaxis ";"
                                    "var save       = " save ";"
                                    "var scatter    = " scatter ";"
                                    "var line       = " line ";"
                                    "var radii          = "  (write-to-string size) ";"
                                    "var linewidth      = "  (write-to-string linewidth) ";"
                                    "var lineopacity    = "  (write-to-string lineopacity) ";"
                                    "var scatteropacity = "  (write-to-string scatteropacity) ";"
                                    "var scattercolor   = "  (concatenate 'string "'" scattercolor "'") ";"
                                    "var linecolor      = "  (concatenate 'string "'" linecolor "'") ";"
                                    "var strokefill     = "  (concatenate 'string "'" strokefill "'") ";"
                                    "var axisX_color    = "  (concatenate 'string "'" axisX_color "'") ";"
                                    "var axisY_color    = "  (concatenate 'string "'" axisY_color "'") ";"
                                    "var axisX_label_color  = " (concatenate 'string "'" axisX_label_color "'") ";"
                                    "var axisY_label_color  = " (concatenate 'string "'" axisY_label_color "'") ";"
                                    "var axisX_tick_color   = " (concatenate 'string "'" axisX_tick_color "'") ";"
                                    "var axisY_tick_color   = " (concatenate 'string "'" axisY_tick_color "'") ";"
                                    "var plotcurve = d3.curve" interpolation ";"
                                    "var squareplot = " squareplot ";"
                                    "var outercolor = " (concatenate 'string "'" outercolor "'") ";"
                                    "var innercolor = " (concatenate 'string "'" innercolor "'") ";"
                                    
                                "</script>"
    "                           <!-- D3JS script for plotting-->
                                <script>
                                        var margin = {top: 10, right: 10, bottom: 10, left: 10}, //See https://bl.ocks.org/mbostock/3019563
                                            padding = {top: 30, right: 30, bottom: 60, left: 60};

                                        if (squareplot){
                                            var outerHeight  = 0.9*Math.min(Math.max(document.documentElement['clientHeight'], document.body['scrollHeight'], document.documentElement['scrollHeight'], document.body['offsetHeight'], document.documentElement['offsetHeight']), Math.max(document.documentElement['clientWidth'], document.body['scrollWidth'], document.documentElement['scrollWidth'], document.body['offsetWidth'], document.documentElement['offsetWidth'])), 
                                                outerWidth   = outerHeight;
                                        } else {
                                            var outerHeight  = 0.9*Math.max(document.documentElement['clientHeight'], document.body['scrollHeight'], document.documentElement['scrollHeight'], document.body['offsetHeight'], document.documentElement['offsetHeight']), 
                                                outerWidth   = 0.9*Math.max(document.documentElement['clientWidth'], document.body['scrollWidth'], document.documentElement['scrollWidth'], document.body['offsetWidth'], document.documentElement['offsetWidth']);
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
                                                .attr('y', 0 - (margin.top / 2))
                                                .attr('text-anchor', 'middle')  
                                                .style('font-size', '18px') 
                                                .style('font-weight', 'bold')  
                                                .text(title);

                                        if (showXaxis){
                                            axisX  = inner.append('g')
                                                            .attr('transform', 'translate(' + 0 + ',' + height + ')')
                                                            .attr('id','xaxis')
                                                            .style('stroke', axisX_label_color)
                                                            .call(d3.axisBottom(Xscale));   

                                            axisX.selectAll('line').style('stroke', axisX_tick_color);             
                                            axisX.selectAll('path').style('stroke', axisX_color);

                                            //Create X axis label
                                            outer.append('text')
                                                .attr('x', innerWidth / 2 )
                                                .attr('y',  innerHeight)
                                                .style('text-anchor', 'middle')
                                                .text(xlab);

                                        }

                                        if (showYaxis){
                                            axisY = inner.append('g')
                                                            .attr('transform', 'translate(' + 0 + ',' + 0 + ')')
                                                            .attr('id','yaxis')
                                                            .style('stroke', axisY_label_color)
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
                                                .text(ylab); 
                                        }
                                        
//--------------------------------------PART THAT NEEDS TO BE IN LISP LOOP              
                                        // Creating path using data in pathinfo and path data generator
                                        if (line){
                                            
                                            // Specify the function for generating path data             
                                            var myline = d3.line()
                                                .x(function(d){return Xscale(d);})
                                                .y(function(d,i){return Yscale(ydata[i]);})
                                                .curve(plotcurve); 

                                            inner.append('path')
                                                .attr('d', myline(xdata))
                                                .style('stroke-width', linewidth)
                                                .style('stroke', linecolor)
                                                .style('opacity',lineopacity)
                                                .style('fill', strokefill);
                                        }

                                        //Add points to plot
                                        if (scatter){
                                            inner.selectAll('scatter-dots')
                                                .data(ydata)  // using the values in the ydata array
                                                .enter().append('svg:circle')  // create a new circle for each value
                                                .attr('cy', function (d) { return Yscale(d); } ) // translate y value to a pixel
                                                .attr('cx', function (d,i) { return Xscale(xdata[i]); } ) // translate x value
                                                .attr('r', radii) // radius of circle
                                                .style('opacity',scatteropacity)
                                                .style('fill', scattercolor);
                                        }
//--------------------------------------END OF PART THAT NEEDS TO BE IN LISP LOOP                                                      
                                    </script>"
                                    "<!-- SCRIPT FOR SAVING; ADAPTED FROM http://bl.ocks.org/Rokotyan/0556f8facbaf344507cdc45dc3622177-->
                                    <script>
                                        //Save button
                                        d3.select('#generate').on('click', function(){
                                            var svgString = getSVGString(outer.node());
                                            svgString2Image( svgString, 2*width, 2*height, fileformat, save ); // passes Blob and filesize String to the callback

                                            function save( dataBlob, filesize ){
                                                saveAs( dataBlob, filename ); // FileSaver.js function
                                            }
                                        });
                                        
                                        // Below are the functions that handle actual exporting:
                                        // getSVGString ( svgNode ) and svgString2Image( svgString, width, height, format, callback )
                                        function getSVGString( svgNode ) {
                                            svgNode.setAttribute('xlink', 'http://www.w3.org/1999/xlink');
                                            var cssStyleText = getCSSStyles( svgNode );
                                            appendCSS( cssStyleText, svgNode );

                                            var serializer = new XMLSerializer();
                                            var svgString = serializer.serializeToString(svgNode);
                                            svgString = svgString.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink='); // Fix root xlink without namespace
                                            svgString = svgString.replace(/NS\d+:href/g, 'xlink:href'); // Safari NS namespace fix

                                            return svgString;

                                            function getCSSStyles( parentElement ) {
                                                var selectorTextArr = [];

                                                // Add Parent element Id and Classes to the list
                                                selectorTextArr.push( '#'+parentElement.id );
                                                for (var c = 0; c < parentElement.classList.length; c++)
                                                        if ( !contains('.'+parentElement.classList[c], selectorTextArr) )
                                                            selectorTextArr.push( '.'+parentElement.classList[c] );

                                                // Add Children element Ids and Classes to the list
                                                var nodes = parentElement.getElementsByTagName('*');
                                                for (var i = 0; i < nodes.length; i++) {
                                                    var id = nodes[i].id;
                                                    if ( !contains('#'+id, selectorTextArr) )
                                                        selectorTextArr.push( '#'+id );

                                                    var classes = nodes[i].classList;
                                                    for (var c = 0; c < classes.length; c++)
                                                        if ( !contains('.'+classes[c], selectorTextArr) )
                                                            selectorTextArr.push( '.'+classes[c] );
                                                }

                                                // Extract CSS Rules
                                                var extractedCSSText = '';
                                                for (var i = 0; i < document.styleSheets.length; i++) {
                                                    var s = document.styleSheets[i];
                                                    
                                                    try {
                                                        if(!s.cssRules) continue;
                                                    } catch( e ) {
                                                            if(e.name !== 'SecurityError') throw e; // for Firefox
                                                            continue;
                                                        }

                                                    var cssRules = s.cssRules;
                                                    for (var r = 0; r < cssRules.length; r++) {
                                                        if ( contains( cssRules[r].selectorText, selectorTextArr ) )
                                                            extractedCSSText += cssRules[r].cssText;
                                                    }
                                                }
                                                

                                                return extractedCSSText;

                                                function contains(str,arr) {
                                                    return arr.indexOf( str ) === -1 ? false : true;
                                                }

                                            }

                                            function appendCSS( cssText, element ) {
                                                var styleElement = document.createElement('style');
                                                styleElement.setAttribute('type','text/css'); 
                                                styleElement.innerHTML = cssText;
                                                var refNode = element.hasChildNodes() ? element.children[0] : null;
                                                element.insertBefore( styleElement, refNode );
                                            }
                                        }


                                        function svgString2Image( svgString, width, height, format, callback ) {
                                            var format = format ? format : 'png';

                                            var imgsrc = 'data:image/svg+xml;base64,'+ btoa( unescape( encodeURIComponent( svgString ) ) ); // Convert SVG string to data URL

                                            var canvas = document.createElement('canvas');
                                            var context = canvas.getContext('2d');

                                            canvas.width = width;
                                            canvas.height = height;

                                            var image = new Image();
                                            image.onload = function() {
                                                context.clearRect ( 0, 0, width, height );
                                                context.drawImage(image, 0, 0, width, height);

                                                canvas.toBlob( function(blob) {
                                                    var filesize = Math.round( blob.length/1024 ) + ' KB';
                                                    if ( callback ) callback( blob, filesize );
                                                });

                                                
                                            };

                                            image.src = imgsrc;
                                        }

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