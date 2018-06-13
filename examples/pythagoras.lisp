;;Pythagorean theorem representation
(setf triangle1x (list 0 4 0 0))
(setf triangle1y (list 0 0 3 0))

(setf triangle2x (list 0 0 3 0))
(setf triangle2y (list 3 7 7 3))

(setf triangle3x (list 3 7 7 3))
(setf triangle3y (list 7 7 4 7))

(setf triangle4x (list 4 7 7 4))
(setf triangle4y (list 0 0 4 0))

(setf triangle5x (list 10 13 13 10))
(setf triangle5y (list 3 3 7 3))

(setf triangle6x (list 13 17 17 13))
(setf triangle6y (list 0 0 3 0))

(setf triangle7x (list 13 17 13 13))
(setf triangle7y (list 0 3 3 0))

(setf triangle8x (list 10 13 10 10))
(setf triangle8y (list 3 7 7 3))

(setf square0x (list 4 7 3 0 4))
(setf square0y (list 0 4 7 3 0))

(setf square1x (list 10 13 13 10 10))
(setf square1y (list 0 0 3 3 0))

(setf square2x (list 13 17 17 13 13))
(setf square2y (list 3 3 7 7 3))

(setf x (list triangle1x triangle2x triangle3x triangle4x triangle5x triangle6x triangle7x triangle8x square0x square1x square2x))
(setf y (list triangle1y triangle2y triangle3y triangle4y triangle5y triangle6y triangle7y triangle8y square0y square1y square2y))
(setf color (list "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#00bfff	" "#00bfff" "#00bfff"))
(setf annotations (list 
    (list "2" 8.15 3.65)
    (list "2" 8.6 3.65)
    (list "2" 9.05 3.65)
    (list "2" 3.65 3.65)
    (list "c" 3.5 3.5)
    (list "b" 1 0.1)
    (list "a" 6 0.1)
    (list "b" 6 6.7)
    (list "a" 1 6.7)
    (list "b" 0.2 6)
    (list "a" 0.2 1)
    (list "a" 6.8 6)
    (list "b" 6.8 1)
    (list "a" 10.2 1.5)
    (list "a" 12.8 1.5)
    (list "a" 11.5 0.1)
    (list "a" 11.5 2.7)
    (list "b" 15 3.1)
    (list "b" 15 6.7)
    (list "b" 13.2 5)
    (list "b" 16.8 5)
    (list "c = a + b" 8.5 3.5)
)
)
(setf annotationcolor (make-list (length annotations) :initial-element "white"))
(setf annotationsize  (make-list (length annotations) :initial-element 14))
(setf (first annotationsize) 8)
(setf (second annotationsize) 8)
(setf (third annotationsize) 8)
(setf (fourth annotationsize) 8)
(setf (car (last annotationcolor)) "black")
(setf (first annotationcolor) "black")
(setf (second annotationcolor) "black")
(setf (third annotationcolor) "black")

(createplot x y 
    :strokefill color
    :title "Visual proof of the Pythagorean theorem"
    :scatter NIL
    :linecolor "white"
    :linewidth 5
    :showXaxis NIL
    :showYaxis NIL
    :annotations annotations
    :annotations-color annotationcolor
    :annotations-fontsize  annotationsize
    :xmin 0 :xmax 17
    :ymin 0 :ymax 8
    :plotheight 500
    :plotwidth 1000
    :squareplot NIL)