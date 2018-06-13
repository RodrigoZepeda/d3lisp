;;This shows all the options available for the user
(setq x (list 1 2 3 4 5))
(setq y (list 1 -2 3 -4 5))
(createplot x y
    :xlab "X axis label" 
    :ylab "Y axis label" 
    :title "Amazing plot title" 
    :size 10                    ;;Size of points in scatter
    :xmin -1                    ;;Minimum of x for plot
    :xmax 7                     ;;Maximum of x for plot
    :ymin -4                    ;;Minimum of y for plot
    :ymax 5                     ;;Maximum of y for plot
    :scattercolor "orange"     ;;Color for points can be HEX or colorname
    :linecolor    "#00bfff"     ;;Color for line can be HEX or colorname 
    :linewidth 5                ;;Width of line 
    :strokefill "none"          ;;Choose a color for closed curves (fills between lines in curve) 
    :lineopacity 1              ;;Alpha parameter between 0 and 1 for opacity of line
    :scatteropacity 1           ;;Alpha parameter between 0 and 1 for opacity of scatter
    :line T                     ;;Plot line 
    :scatter T                  ;;Plot scatter 
    :interpolation "Linear"     ;;Interpolation mode. See other example for details
    :showXaxis T                ;;Show X axis 
    :showYaxis NIL              ;;Hide Y axis 
    :axisX_label_color "white"  ;;Axis label colors
    :axisY_label_color "white"  ;;Axis label colors
    :axisX_tick_color  "white"  ;;Color of ticks on X axis
    :axisY_tick_color  "yellow" ;;Color of ticks on Y axis
    :axisX_color "#F5F5F5"      ;;Color of X axis 
    :axisY_color "#F5F5F5"      ;;Color of Y axis  
    :xaxispos 0                 ;;Where to start X axis 
    :yaxispos 0                 ;;Start axis at origin
    :plotheight "default"       ;;Size of plot height (set to 'default' or choose number)
    :plotwidth 1000             ;;Size of plot width (set to 'default' or choose number)
    :outercolor "black"         ;;Color of outer margin
    :innercolor "#00455c"       ;;Inner margin color
    :annotations (list "The way this starts is amazing" 1 1.4) ;;Annotate plot ('text' x y)
    :annotations-color "white"  ;;Choose annotation color
    :annotations-fontsize 14    ;;Choose annotation size
    :title-fontsize 20          ;;Choose title size
    :title-color "white"        ;;Color of title
    :margin (list 10 10 10 10)  ;;Margin for outer. Order: top, right, bottom, left
    :padding (list 60 30 60 30) ;;Margin for inner. Order: top, right, bottom, left
    :squareplot T               ;;Force plot to be of square form 
    :save T                     ;;Save plot when generated 
    :svgname "advancedoptions"  ;;Filename 
)