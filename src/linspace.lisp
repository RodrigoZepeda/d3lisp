;;;;FUNCTION EQUIVALENT TO MATLAB'S LINSPACE

(defun linspace (xmin xmax &key (lengthout 100))
"
DESCRIPTION:
--------------------------------------------------------
Function for creating a sequence from xmin (double) to
xmax (double) of equally spaced numbers of length
lengthout (integer).
--------------------------------------------------------

INPUT:
--------------------------------------------------------
- xmin: Lower bound of sequence
- xmax: Upper bound of sequence
- lengthout: Total length of sequence

OUTPUT:
--------------------------------------------------------
- x: List of lengthout spaced numbers from xmin to xmax

EXAMPLES:
--------------------------------------------------------
;Create a list of length 100 from 0 to 10
(linspace 0 10)

;Create a list of length 50 from 0 to 10
(linspace 0 10 :lengthout 50)

;Lengthout is rounded to closest upper integer (ceiling)
(linspace 0 10 :lengthout 9.5)

;Linspace also works in reverse order:
(linspace 10 0)

;xmin must be different than xmax
(linspace 2 2)
--------------------------------------------------------

"
    ;Check input values are different
    (unless (/= xmin xmax))
        (error "xmin must be different from xmax")
    )

    ;Convert lengthout to integer
    (setq lengthout (ceiling lengthout))

    ;Calculate the list
    (let ((x (make-list lengthout :initial-element xmin))
          (n (/ (- xmax xmin) (1- lengthout))))
        (loop for i from 1 to (1- lengthout)
            do (setf (nth i x) (+ (nth (1- i) x) n)))
        x))
