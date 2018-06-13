(defun generatevars 
    (x y &key 
        (scatter        (make-list (length x) :initial-element "true")) 
        (line           (make-list (length x) :initial-element "true"))
        (size           (make-list (length x) :initial-element 5))
        (linewidth      (make-list (length x) :initial-element 3)) 
        (lineopacity    (make-list (length x) :initial-element 1)) 
        (scatteropacity (make-list (length x) :initial-element 1)) 
        (scattercolor   (make-list (length x) :initial-element "red")) 
        (linecolor      (make-list (length x) :initial-element "blue")) 
        (strokefill     (make-list (length x) :initial-element "none")) 
        (interpolation  (make-list (length x) :initial-element "Linear")))
    "All variables are lists of length n"
    (let ((javascript_string ""))
        (loop for i from 0 to (1- (length x))
            do (progn
                (setf javascript_string 
                    (concatenate 'string javascript_string
                        "var xdata"         (write-to-string i)   " = "  (to_javascript_array (nth i x)) ";"
                        "var ydata"         (write-to-string i)   " = "  (to_javascript_array (nth i y)) ";"
                        "var scatter"       (write-to-string i)   " = "  (nth i scatter) ";"
                        "var line"          (write-to-string i)   " = "  (nth i line) ";"
                        "var radii"         (write-to-string i)   " = "  (write-to-string (coerce (nth i size) 'single-float)) ";"
                        "var scattercolor"  (write-to-string i)   " = "  (concatenate 'string "'" (nth i scattercolor) "'") ";"
                        "var linecolor"     (write-to-string i)   " = "  (concatenate 'string "'" (nth i linecolor) "'") ";"
                        "var strokefill"    (write-to-string i)   " = "  (concatenate 'string "'" (nth i strokefill) "'") ";"
                        "var plotcurve"     (write-to-string i)   " = "  "d3.curve" (nth i interpolation) ";"     
                        "var linewidth"     (write-to-string i)   " = "  (write-to-string (coerce (nth i linewidth) 'single-float)) ";"
                        "var lineopacity"   (write-to-string i)   " = "  (write-to-string (coerce (nth i lineopacity) 'single-float)) ";"
                        "var scatteropacity"(write-to-string i)   " = "  (write-to-string (coerce (nth i scatteropacity) 'single-float)) ";"
                    )
                )
            )
        )
        javascript_string
    )
)

(defun checklengths (mylist n)
    (if (not (listp mylist))
        (setf mylist (list mylist))
    )
    (if (< (length mylist) n)
        (append mylist (make-list (- n (length mylist)) :initial-element (car (last mylist))))
        mylist
    )
)

(defun generate-lines (n)
    (let ((javascript_string "") (ichar ""))
        (loop for i from 0 to (1- n)
            do (progn
                (setf ichar (write-to-string i))
                (setf javascript_string 
                    (concatenate 'string javascript_string
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
                                .style('stroke-width', linewidth" ichar ")
                                .style('stroke', linecolor" ichar ")
                                .style('opacity',lineopacity" ichar ")
                                .style('fill', strokefill" ichar");
                        }
                        "
                    )
                )
            )
        )
        javascript_string
    )
)

(defun generate-scatters (n)
    (let ((javascript_string "") (ichar ""))
        (loop for i from 0 to (1- n)
            do (progn
                (setf ichar (write-to-string i))
                (setf javascript_string 
                    (concatenate 'string javascript_string
                        "
                        if (scatter" ichar "){
                                        inner.selectAll('scatter-dots')
                                            .data(ydata" ichar ")  // using the values in the ydata0 array
                                            .enter().append('svg:circle')  // create a new circle for each value
                                            .attr('cy', function (d) { return Yscale(d); } ) // translate y value to a pixel
                                            .attr('cx', function (d,i) { return Xscale(xdata0[i]); } ) // translate x value
                                            .attr('r', radii" ichar ") // radius of circle
                                            .style('opacity',scatteropacity" ichar ")
                                            .style('fill', scattercolor" ichar ");
                                    }
                        "
                    )
                )
            )
        )
        javascript_string
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

    (let ((javascript_string ""))
        (if (not (null annotations))
            (loop for i from 0 to (1- (length annotations))
                do (progn
                    (setf javascript_string (concatenate 'string javascript_string 
                        "var annotate" (write-to-string i) " = inner.append('text')
                                            .text('"(first (nth i annotations)) "')
                                            .attr('fill'," (concatenate 'string "'" (nth i annotations-color) "'") ")
                                            .style('font-size'," (write-to-string (coerce  (nth i annotations-fontsize) 'single-float)) ")
                                            .style('font-family', 'sans-serif')
                                            .attr('x', Xscale("  (write-to-string (coerce (second (nth i annotations)) 'single-float)) "))
                                            .attr('y', Yscale("  (write-to-string (coerce (third (nth i annotations)) 'single-float)) "));
                        "
                    ))
                )
            )
        )
        javascript_string
    )
)