;CALL EACH OF THE PACKAGE FUNCTIONS

;Each plot has a different number to make them distinguishable
(defvar *plotnumber* 1)

;Javascript array converts lisp lists to arrays in javascript
(load "to_javascript_array.lisp")

;Creates the d3js (javascript) instructions necessary for htmlcode
(load "generatevars.lisp")

;Creates the html webpage
(load "htmlcode.lisp")

;Function for creating the plot and saving to html
(load "createplot.lisp")

;This is an equivalent for matlab's linspace
(load "linspace.lisp")

;Examples of curves already made
(load "curveexamples.lisp")
