(defun generatevars 
    (x y &key 
        (scatter        (make-list (length x) :initial-element "true")) 
        (line           (make-list (length x) :initial-element "true"))
        (size           (make-list (length x) :initial-element 5))
        (linewidth      (make-list (length x) :initial-element 3)) 
        (lineopacity    (make-list (length x) :initial-element 1)) 
        (scatteropacity (make-list (length x) :initial-element 1)) 
        (scattercolor   (make-list (length x) :initial-element "red")) 
        (linecolor      (make-list (length x) :initial-element "blue")) 
        (strokefill     (make-list (length x) :initial-element "none")) 
        (interpolation  (make-list (length x) :initial-element "Linear")))
    "All variables are lists of length n"
    (let ((javascript_string ""))
        (loop for i from 0 to (1- (length x))
            do (progn
                (setf javascript_string 
                    (concatenate 'string javascript_string
                        "var xdata"         (write-to-string i)   " = "  (to_javascript_array (nth i x)) ";"
                        "var ydata"         (write-to-string i)   " = "  (to_javascript_array (nth i y)) ";"
                        "var scatter"       (write-to-string i)   " = "  (nth i scatter) ";"
                        "var line"          (write-to-string i)   " = "  (nth i line) ";"
                        "var radii"         (write-to-string i)   " = "  (write-to-string (coerce (nth i size) 'single-float)) ";"
                        "var scattercolor"  (write-to-string i)   " = "  (concatenate 'string "'" (nth i scattercolor) "'") ";"
                        "var linecolor"     (write-to-string i)   " = "  (concatenate 'string "'" (nth i linecolor) "'") ";"
                        "var strokefill"    (write-to-string i)   " = "  (concatenate 'string "'" (nth i strokefill) "'") ";"
                        "var plotcurve"     (write-to-string i)   " = "  "d3.curve" (nth i interpolation) ";"     
                        "var linewidth"     (write-to-string i)   " = "  (write-to-string (coerce (nth i linewidth) 'single-float)) ";"
                        "var lineopacity"   (write-to-string i)   " = "  (write-to-string (coerce (nth i lineopacity) 'single-float)) ";"
                        "var scatteropacity"(write-to-string i)   " = "  (write-to-string (coerce (nth i scatteropacity) 'single-float)) ";"
                    )
                )
            )
        )
        javascript_string
    )
)

;(generatevars (list (list 1 2) (list 3 4))  (list (list 1 2) (list 3 4)))

(defun checklengths (mylist n)
    (if (not (listp mylist))
        (setf mylist (list mylist))
    )
    (if (< (length mylist) n)
        (append mylist (make-list (- n (length mylist)) :initial-element (car (last mylist))))
        mylist
    )
)

;(checklengths (list 1 2 3 4) 10)