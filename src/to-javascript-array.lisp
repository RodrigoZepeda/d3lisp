;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: D3 -*-
(in-package #:d3)

;;FUNCTION THAT TRANSFORMS LISP LIST TO JAVASCRIPT ARRAY
(defun to-javascript-array (x)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for transforming a numerical LISP list to a
    Javascript array
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x: A LISP numerical list

    OUTPUT:
    --------------------------------------------------------
    - javascript-array: A Javascript array containing the
    elements of x

    EXAMPLES:
    --------------------------------------------------------
    ;Create an array with numbers 1, 2 and 3
    (to-javascript-array (list 1 2 3))

    ;List elements must be numeric
    (to-javascript-array (list 'a' 2 3))
    --------------------------------------------------------

    "
    (when (and (listp x) (not (null x)))
        (let ((javascript-array "["))
            (dolist (xelem (butlast x) javascript-array)
                (setf javascript-array
                  (concatenate 'string javascript-array
                    (write-to-string (coerce xelem 'single-float)) ",")))
            (setf javascript-array
                (concatenate 'string javascript-array (write-to-string
                  (coerce (car (last x)) 'single-float)) "]")))))
