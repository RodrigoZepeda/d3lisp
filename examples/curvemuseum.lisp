;Creation of curve museum
(setq x1 (first  (hipocycloid 3 1  :center (list -15 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setq y1 (second (hipocycloid 3 1  :center (list -15 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setf a1 (list "Deltoid" -15 3))

(setq x2 (first  (hipocycloid 3 (/ 3 4)  :center (list 0 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setq y2 (second (hipocycloid 3 (/ 3 4)  :center (list 0 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setf a2 (list "Astroid" 0 3))

(setq x3 (first  (hipocycloid 4.1 1.1 :center (list 15 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y3 (second (hipocycloid 4.1 1.1 :center (list 15 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a3 (list "Hipocycloid" 15 3))

(setq x4 (first  (epicycloid 1 1 :center (list -15 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y4 (second (epicycloid 1 1 :center (list -15 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a4 (list "Cardioid" -15 17))

(setq x5 (first  (epicycloid 2 1 :center (list 0 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y5 (second (epicycloid 2 1 :center (list 0 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a5 (list "Nephroid" 0 17))

(setq x6 (first  (epicycloid 2.1 1.1 :center (list 15 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y6 (second (epicycloid 2.1 1.1 :center (list 15 25) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a6 (list "Epicycloid" 15 17))

(setq x7 (first  (involute 0.1 :center (list -15 40) :angle-points (linspace 0 (* 10 pi) :lengthout 1000))))
(setq y7 (second (involute 0.1 :center (list -15 40) :angle-points (linspace 0 (* 10 pi) :lengthout 1000))))
(setf a7 (list "Involute" -15 34))

(setq x8 (first  (bicorn 4 :center (list 0 38))))
(setq y8 (second (bicorn 4 :center (list 0 38))))
(setf a8 (list "Bicorn" 0 34))

(setq x9 (first  (archimedes 0.1 :center (list 15 40) :angle-points (linspace 0 (* 10 pi) :lengthout 1000))))
(setq y9 (second (archimedes 0.1 :center (list 15 40) :angle-points (linspace 0 (* 10 pi) :lengthout 1000))))
(setf a9 (list "Archimedes spiral" 15 34))

(setq x10 (first  (epitrochoid 2.1 1.1  :d 1.5 :center (list -15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y10 (second (epitrochoid 2.1 1.1  :d 1.5 :center (list -15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a10 (list "Epitrochoid" -15 47))

(setq x11 (first  (ngon 100 :r 3 :center (list 0 55))))
(setq y11 (second (ngon 100 :r 3 :center (list 0 55))))
(setf a11 (list "Circle" 0 47))

(setq x12 (first  (hipotrochoid 3.1 1.1  :d 1.5 :center (list 15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y12 (second (hipotrochoid 3.1 1.1  :d 1.5 :center (list 15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a12 (list "Hipotrochoid" 15 47))

;Creation of epicicloid
(createplot (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 
            (list y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) 
            :title "Curve museum" :scatter NIL
            :title-fontsize 25
            :linewidth 1
            :annotations (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            :padding (list 50 30 50 30)
            :xmin -30 :xmax 30 :ymin 5 :ymax 60
            :linecolor "darkmagenta"
            :squareplot T :interpolation "CatmullRom" 
            :showXaxis NIL :showYaxis NIL :squareplot T)