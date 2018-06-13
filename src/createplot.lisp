(defun createplot  (x y &key (xlab "My x label") (ylab "My y label") (title "My Title") (size 7) 
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
                            (port 1234) (pagename (concatenate 'string "plot" (write-to-string *plotnumber*))))

    ;;Update plot number
    (setq *plotnumber* (1+ *plotnumber*))

    ;;Create new port to connect to browser
    (if *createinstance*
        (progn 
            (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))
            (setq *createinstance* NIL)
        )
    )

    ;;Call function
    (push (hunchentoot:create-prefix-dispatcher (concatenate 'string "/" pagename) 
        (lambda () 
            (htmlcode x y 
                :xlab xlab :ylab ylab :title title :size size 
                :xmin xmin :ymin ymin :xmax xmax :ymax ymax :scattercolor scattercolor
                :linecolor linecolor :linewidth linewidth :strokefill strokefill :line line :scatter scatter
                :interpolation interpolation :showXaxis showXaxis :showYaxis showYaxis 
                :axisX_label_color axisX_label_color :axisY_label_color axisY_label_color
                :axisX_tick_color axisX_tick_color :axisY_tick_color axisY_tick_color :axisX_color axisX_color 
                :axisY_color axisY_color :lineopacity lineopacity :scatteropacity scatteropacity 
                :xaxispos xaxispos :yaxispos yaxispos
                :plotheight plotheight :plotwidth plotwidth
                :outercolor outercolor :innercolor innercolor :annotations annotations
                :annotations-color annotations-color :annotations-fontsize annotations-fontsize
                :title-fontsize title-fontsize :title-color title-color
                :margin margin :padding padding
                :squareplot squareplot :save save :svgname svgname 
                :plotnum (1- *plotnumber*)
            ))) 
        hunchentoot:*dispatch-table*)

    ;;Open browser to run program
    ;;COMMENT THIS IF NOT ON MAC OR NOT ON SBCL IF THIS IS THE CASE, OPEN YOUR BROWSER MANUALLY AND GO TO
    ;;localhost port / pagename (default is localhost:1234/plot#number_of_plots_since_started). Example,
    ;;for second plot it is found in http://localhost:1234/plot2
    (uiop:run-program 
        (concatenate 'string "open http://localhost:" (write-to-string port) "/" pagename))

)