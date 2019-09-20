;Example of scatterplot
(let ((x (loop for i from 0 below 10 collect (random 101)))
      (y (loop for i from 0 below 10 collect (random 101))))
     (plot x y :title "Scatterplot" :x-label "x" :y-label "y" :line NIL
      :size 20 :scatter-color "purple"))
