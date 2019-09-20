;Plot two variables at same time
(let* ((x (list (range 0 10) (range 0 10)))
       (y (list (mapcar #'sin (first x)) (mapcar #'cos (second x)))))
       (plot x y :title "My favorite trigonometric functions"
                 :x-label "x" :y-label "f(x)"
                 :scatter NIL
                 :interpolation "CatmullRom"
                 :line-color   (list "tomato" "steelblue")
                 :annotations (list (list "cos(x)" (/ (* 3 pi) 2) -1.1) (list "sin(x)" pi -1.1))
                 :annotations-color (list "tomato" "steelblue")
                 :annotations-font-size 15))
