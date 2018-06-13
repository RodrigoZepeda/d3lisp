;;Creates a plot of 50 shades of gray
(setf n 50) ;;Number of points of gray plot
(setf x       (make-list n))
(setf y       (make-list n))
(setf color   (make-list n))
(setf colname (make-list n))
(let ((k -0.5) (mystep (/ 255 (1- n))) (currentstep 0))
    (loop for i from 0 to (1- n)
        do (progn 
                (if (= (mod i 5) 0) (incf k))
                (setf (nth i x) (list (+ 3 (- (mod i 5) 0.5)) (+ 3 (- (mod i 5) 0.5)) (+ 3 (+ (mod i 5) 0.5)) (+ 3 (+ (mod i 5) 0.5)) (+ 3 (- (mod i 5) 0.5))))
                (setf (nth i y) (list (- k 0.5) (+ k 0.5) (+ k 0.5) (- k 0.5) (- k 0.5)))
                (setf currentstep (write-to-string (truncate (* i mystep))))
                (setf (nth i color) (concatenate 'string "rgb(" currentstep "," currentstep "," currentstep ")"))
                (setf (nth i colname) (list (nth i color) (+ 3 (mod i 5)) k))
            )
    )
)

(createplot x y  
        :title "50 Shades of Gray"
        :scatter NIL
        :linecolor "none"
        :annotations-fontsize 8
        :xmin -0.5 :xmax 10
        :ymin 0 :ymax 10.5
        :annotations colname
        :annotations-color (reverse color)
        :showXaxis NIL :showYaxis NIL
        :squareplot T
        :padding (list 50 10 20 10)
        :margin  (list 40 10 10 10)
        :plotheight 1000
        :strokefill color)

