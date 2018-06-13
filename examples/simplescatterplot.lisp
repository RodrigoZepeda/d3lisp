;Example of scatterplot
(setq x (mapcar (lambda (x) (random 100.0)) (make-list 500)))
(setq y (mapcar (lambda (x) (random 100.0)) (make-list 500)))
(createplot x y :title "Scatterplot" :xlab "x" :ylab "y" :line NIL :size 5 :scattercolor "purple")
