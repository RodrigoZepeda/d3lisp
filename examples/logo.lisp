;CREATE d3lisp LOGO
(let ((x1 (first  (epitrochoid 2.17 1.13 :d 1.5 :angle-points
            (range 0 (* 50 pi) :length-out 1000))))
      (y1 (second (epitrochoid 2.17 1.13  :d 1.5 :angle-points
            (range 0 (* 50 pi) :length-out 1000))))
      (x2 (first  (ngon 100 :r 1.5)))
      (y2 (second (ngon 100 :r 1.5)))
      (x3 (first  (ngon 100 :r 4.8)))
      (y3 (second (ngon 100 :r 4.8))))
     (plot (list x1 x2 x3) (list y1 y2 y3)
      :title ""
      :show-x-axis NIL
      :line-width (list 3 10 5)
      :line-color (list "purple" "black" "#4E004E")
      :annotations (list "d3lisp" 0 -0.2)
      :annotations-font-size 25
      :annotations-color "black"
      :square-plot T
      :scatter NIL
      :show-y-axis NIL
      :save T
      :svg-name "logo"
      :interpolation "Basis"))
