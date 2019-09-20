;;Pythagorean theorem representation
(let* ((triangle1x (list 0 4 0 0))
       (triangle1y (list 0 0 3 0))

       (triangle2x (list 0 0 3 0))
       (triangle2y (list 3 7 7 3))

       (triangle3x (list 3 7 7 3))
       (triangle3y (list 7 7 4 7))

       (triangle4x (list 4 7 7 4))
       (triangle4y (list 0 0 4 0))

       (triangle5x (list 10 13 13 10))
       (triangle5y (list 3 3 7 3))

       (triangle6x (list 13 17 17 13))
       (triangle6y (list 0 0 3 0))

       (triangle7x (list 13 17 13 13))
       (triangle7y (list 0 3 3 0))

       (triangle8x (list 10 13 10 10))
       (triangle8y (list 3 7 7 3))

       (square0x (list 4 7 3 0 4))
       (square0y (list 0 4 7 3 0))

       (square1x (list 10 13 13 10 10))
       (square1y (list 0 0 3 3 0))

       (square2x (list 13 17 17 13 13))
       (square2y (list 3 3 7 7 3))

       (x (list triangle1x triangle2x triangle3x triangle4x triangle5x triangle6x
                triangle7x triangle8x square0x square1x square2x))
       (y (list triangle1y triangle2y triangle3y triangle4y triangle5y triangle6y
                triangle7y triangle8y square0y square1y square2y))

       (color (list "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d" "#0d370d"
                    "#0d370d" "#0d370d" "#00bfff" "#00bfff" "#00bfff"))

       (annotations (list (list "2" 8.15 3.65)
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
                          (list "c = a + b" 8.5 3.5)))

         (annotationcolor (make-list (length annotations) :initial-element "white"))
         (annotationsize  (make-list (length annotations) :initial-element 14)))

         (setf (first annotationsize) 8)
         (setf (second annotationsize) 8)
         (setf (third annotationsize) 8)
         (setf (fourth annotationsize) 8)
         (setf (car (last annotationcolor)) "black")
         (setf (first annotationcolor) "black")
         (setf (second annotationcolor) "black")
         (setf (third annotationcolor) "black")

        (plot x y
            :stroke-fill color
            :title "Visual proof of the Pythagorean theorem"
            :scatter NIL
            :line-color "white"
            :line-width 5
            :show-x-axis NIL
            :show-y-axis NIL
            :annotations annotations
            :annotations-color annotationcolor
            :annotations-font-size  annotationsize
            :x-minimum 0 :x-maximum 17
            :y-minimum 0 :y-maximum 8
            :plot-height 500
            :plot-width 1000
            :square-plot NIL))
