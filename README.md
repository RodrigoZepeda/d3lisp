d3lisp: A d3js interface for plotting on LISP
================

## Setup

**d3lisp** is a [d3js](https://d3js.org/) interface for creating plots
in ANSI Common LISP. To run it you need to:

1.  Download the project (either `git clone
    https://github.com/RodrigoZepeda/d3lisp/` or click on `Download`
    button). We will call the directory where the project kept as
    `"path/to/project"`.
2.  (Optional) Set up a directory with all the files, in LISP:

<!-- end list -->

    (setf *default-pathname-defaults* (truename "path/to/project/src"))

3.  Load `setup.lisp` to set up the functions:

<!-- end list -->

    (load "setup.lisp")

> Currently **d3lisp** has been tested in the following LISP versions:
> 1. CLISP
> 
> 2.  SBCL

## Example

The function for creating the plots is `createplot`. Here, for example,
we plot the exponential function:

    (let* (
            (x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
            (y (mapcar #'exp x))
          )
          (createplot x y :title "Exponential"  :squareplot T  
                          :title-fontsize 100 
                          :margin (list 10 10 10 10)
                          :padding (list 150 30 60 60) 
                          :xlab "x" :ylab "exp(x)" 
                          :interpolation "MonotoneX"
                          :filename "MyFirstPlot")
    )

Which results in the following image:

<center>

<img src="./examples/exponential.svg">

</center>

The complete code example assuming `d3lisp` is in `Desktop` is as
follows:

    ;;Set directory to functions
    (setf *default-pathname-defaults* (truename "~/Desktop/d3lisp/src"))
    
    ;;Load functions
    (load "setup.lisp")
    
    ;;Create plot
    (let* (
            (x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
            (y (mapcar #'exp x))
          )
          (createplot x y :title "Exponential"  :squareplot T  
                          :title-fontsize 100 
                          :margin (list 10 10 10 10)
                          :padding (list 150 30 60 60) 
                          :xlab "x" :ylab "exp(x)" 
                          :interpolation "MonotoneX"
                          :filename "MyFirstPlot")
    )

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
