d3lisp: A d3js interface for plotting on LISP
================

Setup
-----

**d3lisp** is a [d3js](https://d3js.org/) interface for creating plots in LISP. To run it you need to:

1.  Install [quicklisp](https://www.quicklisp.org/beta/)
2.  Use quicklisp to install [hunchentoot](https://edicl.github.io/hunchentoot/) (see here for [SBCL installation on Mac](https://medium.com/@m2k/web-server-on-mac-os-x-via-common-lisp-prism-6e94ef178c1c))
3.  Have a browser and a persistent Internet connection

> Currently **d3lisp** only runs on Mac

One you have installed and set up a directory with all the files, in SBCL:

    (setf *default-pathname-defaults* (truename "path/to/project"))

Load `setup.lisp` to set up the functions:

    (load "setup.lisp")

Example
-------

The function for creating the plots is `createplot`. Here, for example, we plot the sine function:

    (setq x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
    (setq y (mapcar #'exp x))
    (createplot x y :title "Exponential"  :squareplot T  
                    :title-fontsize 100 
                    :margin (list 10 10 10 10)
                    :padding (list 150 30 60 60) 
                    :xlab "x" :ylab "exp(x)" 
                    :interpolation "MonotoneX"
    )

Which results in the following image:
<center>
<img src="./examples/exponential.svg">
</center>
You can see additional examples on the [example website](https://rodrigozepeda.github.io/d3lisp/index.html).
