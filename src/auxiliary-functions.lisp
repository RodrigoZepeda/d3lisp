;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

;; AUXILIARY FUNCTIONS FOR PLOTTING

;; FUNCTION EQUIVALENT TO MATLAB'S linspace or PYTHON and JULIA's range.
(defun range (x-minimum x-maximum &key (length-out 100))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating a sequence from x-minimum (double) to
    x-maximum (double) of equally spaced numbers of length
    length-out (integer).
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - x-minimum: Lower bound of sequence
    - x-maximum: Upper bound of sequence
    - length-out: Total length of sequence

    OUTPUT:
    --------------------------------------------------------
    - x: List of length-out spaced numbers from x-minimum to x-maximum

    EXAMPLES:
    --------------------------------------------------------
    ;Create a list of length 100 from 0 to 10
    (range 0 10)

    ;Create a list of length 50 from 0 to 10
    (range 0 10 :length-out 50)

    ;length-out is rounded to closest upper integer (ceiling)
    (range 0 10 :length-out 9.5)

    ;range also works in reverse order:
    (range 10 0)

    ;x-minimum must be different than x-maximum
    (range 2 2)
    --------------------------------------------------------

    "
    ;Check input values are different
    (unless (/= x-minimum x-maximum)
        (error "x-minimum must be different from x-maximum")
    )

    ;Convert length-out to integer
    (setf length-out (ceiling length-out))

    ;Calculate the list
    (let ((x (make-list length-out :initial-element x-minimum))
          (n (/ (- x-maximum x-minimum) (1- length-out))))
        (loop for i from 1 to (1- length-out)
            do (setf (nth i x) (+ (nth (1- i) x) n)))
        x))

;;FUNCTION FOR INCREASING THE LENGTH OF A LIST TO N
(defun increase-list-length (my-list n)
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for increasing the length of a list by copying
    its last element until reaching length n
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - my-list: A list to increase length until n if (length my-list)
               is smaller than n.
    - n:       Desired length of list

    OUTPUT:
    --------------------------------------------------------
    - my-list: List of length >= n

    EXAMPLES:
    --------------------------------------------------------
    ;Don't change list if it is of length n
    (increase-list-length (range 0 10 :length-out 10) 10)

    ;Change list if it is of length smaller than n
    (increase-list-length (range 0 10 :length-out 10) 20)

    ;Don't change list if it is of length > n
    (increase-list-length (range 0 10 :length-out 20) 10)
    --------------------------------------------------------

    "
    (when (not (listp my-list))
        (setf my-list (list my-list)))
    (if (< (length my-list) n)
        (append my-list (make-list (- n (length my-list)) :initial-element (car (last my-list))))
        my-list))
