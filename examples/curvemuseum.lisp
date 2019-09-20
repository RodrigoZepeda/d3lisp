;Creation of curve museum
(let ((x1 (first  (hipocycloid 3 1  :center (list -15 10) :angle-points (range 0 (* 2 pi) :length-out 1000))))
      (y1 (second (hipocycloid 3 1  :center (list -15 10) :angle-points (range 0 (* 2 pi) :length-out 1000))))
      (a1 (list "Deltoid" -15 3))

      (x2 (first  (hipocycloid 3 (/ 3 4)  :center (list 0 10) :angle-points (range 0 (* 2 pi) :length-out 1000))))
      (y2 (second (hipocycloid 3 (/ 3 4)  :center (list 0 10) :angle-points (range 0 (* 2 pi) :length-out 1000))))
      (a2 (list "Astroid" 0 3))

      (x3 (first  (hipocycloid 4.1 1.1 :center (list 15 10) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y3 (second (hipocycloid 4.1 1.1 :center (list 15 10) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a3 (list "Hipocycloid" 15 3))

      (x4 (first  (epicycloid 1 1 :center (list -15 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y4 (second (epicycloid 1 1 :center (list -15 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a4 (list "Cardioid" -15 17))

      (x5 (first  (epicycloid 2 1 :center (list 0 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y5 (second (epicycloid 2 1 :center (list 0 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a5 (list "Nephroid" 0 17))

      (x6 (first  (epicycloid 2.1 1.1 :center (list 15 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y6 (second (epicycloid 2.1 1.1 :center (list 15 25) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a6 (list "Epicycloid" 15 17))

      (x7 (first  (involute 0.1 :center (list -15 40) :angle-points (range 0 (* 10 pi) :length-out 1000))))
      (y7 (second (involute 0.1 :center (list -15 40) :angle-points (range 0 (* 10 pi) :length-out 1000))))
      (a7 (list "Involute" -15 34))

      (x8 (first  (bicorn 4 :center (list 0 38))))
      (y8 (second (bicorn 4 :center (list 0 38))))
      (a8 (list "Bicorn" 0 34))

      (x9 (first  (archimedes 0.1 :center (list 15 40) :angle-points (range 0 (* 10 pi) :length-out 1000))))
      (y9 (second (archimedes 0.1 :center (list 15 40) :angle-points (range 0 (* 10 pi) :length-out 1000))))
      (a9 (list "Archimedes spiral" 15 34))

      (x10 (first  (epitrochoid 2.1 1.1  :d 1.5 :center (list -15 55) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y10 (second (epitrochoid 2.1 1.1  :d 1.5 :center (list -15 55) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a10 (list "Epitrochoid" -15 47))

      (x11 (first  (ngon 100 :r 3 :center (list 0 55))))
      (y11 (second (ngon 100 :r 3 :center (list 0 55))))
      (a11 (list "Circle" 0 47))

      (x12 (first  (hipotrochoid 3.1 1.1  :d 1.5 :center (list 15 55) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (y12 (second (hipotrochoid 3.1 1.1  :d 1.5 :center (list 15 55) :angle-points (range 0 (* 50 pi) :length-out 1000))))
      (a12 (list "Hipotrochoid" 15 47)))

      ;Creation of epicicloid
      (plot (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
            (list y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12)
            :title "Curve museum" :scatter NIL
            :title-font-size 25
            :line-width 1
            :annotations (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            :padding (list 50 30 50 30)
            :x-minimum -30 :x-maximum 30 :y-minimum 5 :y-maximum 60
            :line-color "darkmagenta"
            :square-plot T :interpolation "CatmullRom"
            :show-x-axis NIL :show-y-axis NIL :square-plot T))
