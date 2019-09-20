
<!--html_preserve-->

<div style="text-align:center;">

<img src="examples/logo.svg" width = "25%;">

<h1>

d3lisp: A d3js interface for plotting on LISP

</h1>

</div>

<!--/html_preserve-->

## TL;DR

See the example gallery at
<https://rodrigozepeda.github.io/d3lisp/index.html#examples_for_getting_started>
.

## Setup

**d3lisp** is a [d3js](https://d3js.org/) interface for creating plots
in ANSI Common LISP. To run it you need to:

1.  Download the project (either `git clone
    https://github.com/RodrigoZepeda/d3lisp/` or click on `Download`
    button).
2.  Load the `asdf` package. See
    [here](http://www.sbcl.org/asdf/Using-asdf-to-load-systems.html) for
    loading packages on SBCL for example.

<!-- end list -->

    (asdf:load-system :d3)
    (use-package 'd3)

> Currently **d3lisp** has been tested in the following LISP versions:
> 1. SBCL

## Example

The function for creating the plots is `plot`. Here, for example, we
plot the exponential function:

    (let* ((x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
           (y (mapcar #'exp x)))
          (plot x y :title "Exponential"  :square-plot T  
                          :title-font-size 100 
                          :margin (list 10 10 10 10)
                          :padding (list 150 30 60 60) 
                          :x-label "x" :y-label "exp(x)" 
                          :interpolation "MonotoneX"
                          :file-name "MyFirstPlot"))

Which results in the following image:

<center>

<img src="./examples/exponential.svg">

</center>

The complete code example is as follows:

    ;;Set directory to functions
    (asdf:load-system :d3)
    (use-package 'd3)
    
    ;;Create plot
    (let* ((x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
           (y (mapcar #'exp x)))
          (plot x y :title "Exponential"  :square-plot T  
                          :title-font-size 100 
                          :margin (list 10 10 10 10)
                          :padding (list 150 30 60 60) 
                          :x-label "x" :y-label "exp(x)" 
                          :interpolation "MonotoneX"
                          :file-name "MyFirstPlot"))

The plot is saved to a file called `MyFirstPlot.html` in your current
directory.

You can see additional examples on the [example
website](https://rodrigozepeda.github.io/d3lisp/index.html) .

## License

The project is released under an [MIT
License](https://github.com/RodrigoZepeda/d3lisp/blob/master/LICENSE).

## Contributing

Feel free to contribute and discuss any changes you want to make. See
the [code of conduct
here](https://github.com/RodrigoZepeda/d3lisp/blob/master/CONTRIBUTE.md).
