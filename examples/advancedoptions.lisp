;;This shows all the options available for the user
(let ((x (list 1 2 3 4 5))
      (y (list 1 -2 3 -4 5)))
  (plot x y
      :x-label "X axis label"
      :y-label "Y axis label"
      :title "Amazing plot title"
      :size 10                    ;;Size of points in scatter
      :x-minimum -1               ;;Minimum of x for plot
      :x-maximum 7                ;;Maximum of x for plot
      :y-minimum -4               ;;Minimum of y for plot
      :y-maximum 5                ;;Maximum of y for plot
      :scatter-color "orange"     ;;Color for points can be HEX or colorname
      :line-color    "#00bfff"    ;;Color for line can be HEX or colorname
      :line-width 5               ;;Width of line
      :stroke-fill "none"         ;;Choose a color for closed curves (fills between lines in curve)
      :line-opacity 1             ;;Alpha parameter between 0 and 1 for opacity of line
      :scatter-opacity 1          ;;Alpha parameter between 0 and 1 for opacity of scatter
      :line T                     ;;Plot line
      :scatter T                  ;;Plot scatter
      :interpolation "Linear"     ;;Interpolation mode. See other example for details
      :show-x-axis T              ;;Show X axis
      :show-y-axis NIL            ;;Hide Y axis
      :axis-x-label-color"white"  ;;Axis label colors
      :axis-y-label-color "white" ;;Axis label colors
      :axis-x-tick-color  "white" ;;Color of ticks on X axis
      :axis-y-tick-color  "yellow";;Color of ticks on Y axis
      :axis-x-color "#F5F5F5"     ;;Color of X axis
      :axis-y-color "#F5F5F5"     ;;Color of Y axis
      :x-axis-position 0          ;;Where to start X axis
      :y-axis-position 0          ;;Start axis at origin
      :plot-height "default"      ;;Size of plot height (set to 'default' or choose number)
      :plot-width 1000            ;;Size of plot width (set to 'default' or choose number)
      :outer-background-color "black"   ;;Color of outer margin
      :inner-background-color "#00455c" ;;Inner margin color
      :annotations (list "The way this starts is amazing" 1 1.4) ;;Annotate plot ('text' x y)
      :annotations-color "white"  ;;Choose annotation color
      :annotations-font-size 14   ;;Choose annotation size
      :title-font-size 20         ;;Choose title size
      :title-color "white"        ;;Color of title
      :margin (list 10 10 10 10)  ;;Margin for outer. Order: top, right, bottom, left
      :padding (list 60 30 60 30) ;;Margin for inner. Order: top, right, bottom, left
      :square-plot T              ;;Force plot to be of square form
      :save T                     ;;Save plot when generated
      :svg-name "advancedoptions" ;;SVG file name when downloading
      :file-name "MyHTML"))       ;;html file name when saving
