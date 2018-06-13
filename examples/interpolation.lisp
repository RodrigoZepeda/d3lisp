;Interpolation examples
(setq x (make-list 8 :initial-element (linspace 0 10 :lengthout 5)))
(setq y (make-list 8 :initial-element (mapcar #'sin (first x))))
(createplot x y :title "Interpolation examples" :xlab "x" :ylab "sin"
    :interpolation (list "Linear" "Step" "StepBefore" "StepAfter" "Basis" "Cardinal" "MonotoneX" "CatmullRom")
    :annotations  (list (list "Linear"      1 -1.1) 
                        (list "Step"        2 -1.1) 
                        (list "StepBefore"  3 -1.1) 
                        (list "StepAfter"   4 -1.1) 
                        (list "Basis"       5 -1.1) 
                        (list "Cardinal"    6 -1.1) 
                        (list "MonotoneX"   7 -1.1)
                        (list "CatmullRom"  8 -1.1))
    :annotations-color (list "red" "blue" "green" "orange" "steelblue" "purple" "pink" "Gray")
    :linecolor (list "red" "blue" "green" "orange" "steelblue" "purple" "pink" "Gray")
    :lineopacity 0.5
    :scattercolor "black"
    :scatter T
)

