;Simplest example
(let ((x (list 10 15 30 50))
      (y (list 1 2 3 4)))
     (plot x y :title "Awesome plot"
               :x-label "Time since I started using LISP"
               :y-label "Number of extra parentheses in my life"))
