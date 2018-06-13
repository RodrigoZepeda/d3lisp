;Plot two variables at same time
(setq x (list (linspace 0 10) (linspace 0 10)))
(setq y (list (mapcar #'sin (first x)) (mapcar #'cos (second x))))
(createplot x y :title "My favourite trigonometric functions"  
                :xlab "x" :ylab "f(x)"
                :scatter NIL
                :interpolation "CatmullRom"
                :linecolor   (list "tomato" "steelblue") 
                :annotations (list (list "cos(x)" (/ (* 3 pi) 2) -1.1) (list "sin(x)" pi -1.1))
                :annotations-color (list "tomato" "steelblue") 
                :annotations-fontsize 15)