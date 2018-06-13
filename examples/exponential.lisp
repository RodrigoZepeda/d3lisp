;Exponential example using linspace
(setq x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
(setq y (mapcar #'exp x))
(createplot x y :title "Exponential"  :squareplot T  :title-fontsize 100 :margin (list 10 10 10 10)
    :padding (list 150 30 60 60) :xlab "x" :ylab "exp(x)" :interpolation "MonotoneX"
    :scattercolor "red" :linecolor "black"
)