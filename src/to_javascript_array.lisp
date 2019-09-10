;;;FUNCTION THAT TRANSFORMS LISP LIST TO JAVASCRIPT ARRAY

(defun to_javascript_array (x)
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
- javascript_array: A Javascript array containing the
elements of x

EXAMPLES:
--------------------------------------------------------
;Create an array with numbers 1, 2 and 3
(to_javascript_array (list 1 2 3))

;List elements must be numeric
(to_javascript_array (list 'a' 2 3))
--------------------------------------------------------

"
    (if (and (listp x) (not (null x)))
        (let ((javascript_array "["))
            (dolist (xelem (butlast x) javascript_array)
                (setq javascript_array
                  (concatenate 'string javascript_array
                    (write-to-string (coerce xelem 'single-float)) ",")))
            (setf javascript_array
                (concatenate 'string javascript_array (write-to-string
                  (coerce (car (last x)) 'single-float)) "]")))
        (error "Input X must be a non-empty list")))
