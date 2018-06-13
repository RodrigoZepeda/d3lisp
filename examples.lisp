(setf *default-pathname-defaults* (truename "/Users/insp/Dropbox/UNAM_POSGRADO/CALC SIMB/proyecto/lispplot/"))
(load "setup.lisp")

;Para evitar errores en sbcl
(defvar x NIL)
(defvar y NIL)

;Ejemplo básico de una gráfica
(setq x (list 1 2 3 4))
(setq y (list 1 -2 3 -4))
(createplot x y :title "Mi gráfica cool" :xlab "Etiqueta x" :ylab "Etiqueta y")

;Ejemplo de uso de linspace
(setq x (list 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 5))
(setq y (mapcar #'exp x))
(createplot x y :title "Exponential"  :squareplot T  :title-fontsize 100 :margin (list 10 10 10 10)
    :padding (list 150 30 60 60) :xlab "x" :ylab "exp(x)" :interpolation "MonotoneX"
)

;Gráfica doble
(setq x (list (linspace 0 10) (linspace 0 10)))
(setq y (list (mapcar #'sin (first x)) (mapcar #'cos (second x))))
(createplot x y :title "My favourite trigonometric functions"  :squareplot NIL :scatter (list NIL NIL) 
                :linecolor (list "red" "blue") :annotations (list (list "cos" pi -1) (list "sin" (/ pi 2) 1))
                :annotations-fontsize 15)

;Ejemplo de interpolación
(setq x (list (* -1 pi)  (/ (* -1 pi) 2) (/ (* -1 pi) 4) (/ (* -1 pi) 8) 0 (/ pi 8) (/ pi 4) (/ pi 2) pi))
(setq y (mapcar #'sin x))
(createplot x y :title "Gráfica de seno" :xlab "x" :ylab "sin")

;Ejemplo de interpolación
(setq x (list (* -1 pi)  (/ (* -1 pi) 2) (/ (* -1 pi) 4) (/ (* -1 pi) 8) 0 (/ pi 8) (/ pi 4) (/ pi 2) pi))
(setq y (mapcar #'sin x))
(createplot x y :title "Gráfica de seno" :xlab "x" :ylab "y" :interpolation "MonotoneX")

;Ejemplo de un scatter sin líneas
(setq x (make-list 500))
(setq x (mapcar (lambda (x) (random 100.0)) x))
(setq y (mapcar (lambda (x) (random 100.0)) x))
(createplot x y :title "Gráfica de seno" :xlab "x" :ylab "y" :interpolation "MonotoneX" :line NIL :size 3 :scattercolor "purple")

;Regular ngon
(setq x (first (ngon 12)))
(setq y (second (ngon 12)))
(createplot x y :title "ngon"  :scatter T :squareplot T)

;Two poligons
(setq x (list (first  (ngon 5 :center (list -2 1))) (first (ngon 10 :center (list 1 1)))))
(setq y (list (second (ngon 5 :center (list -2 1))) (second (ngon 10 :center (list 1 1)))))
(createplot x y :title "ngon"  :scatter NIL :squareplot T :xmin -3 :xmax 3 :ymin -3 :ymax 3)

;Poligon options
(setq x (first (ngon 5)))
(setq y (second (ngon 5)))
(createplot x y :title "Pentagon" :scatter NIL :strokefill "orange" :squareplot T :showXaxis NIL :showYaxis NIL :save  T :filename "Pentagon" :fileformat "jpeg")

;Circle as poligon
(setq x (first (ngon 100)))
(setq y (second (ngon 100)))
(createplot x y :title "Circle"  :scatter NIL :squareplot T :interpolation "CatmullRom")

;Hipotrochoid
(setq x1 (first (hipotrochoid 37 13 :d 13 :center (list -10 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y1 (second (hipotrochoid 37 13 :d 13 :center (list -10 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq x2 (first (epitrochoid 37 13 :d 13 :center (list 10 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y2 (second (epitrochoid 37 13 :d 13 :center (list 10 10) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(createplot (list x1 x2) (list y1 y2) :title "Hipotrochoid" :linecolor (list "red" "blue") :scatter NIL :squareplot T :interpolation "CatmullRom")

;Create cycloids

;Creation of deltoid
(setq x1 (first  (hipocycloid 3 1  :center (list -15 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setq y1 (second (hipocycloid 3 1 :center (list -15 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setf a1 (list "Deltoid" -15 3))

(setq x2 (first  (hipocycloid 3 (/ 3 4)  :center (list 0 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
(setq y2 (second (hipocycloid 3 (/ 3 4) :center (list 0 10) :angle-points (linspace 0 (* 2 pi) :lengthout 1000))))
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

(setq x12 (first  (hipotrochoid 2.1 1.1  :d 1.5 :center (list 15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y12 (second (hipotrochoid 2.1 1.1  :d 1.5 :center (list 15 55) :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setf a12 (list "Hipotrochoid" 15 47))

;Creation of epicicloid
(createplot (list x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 
            (list y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) 
            :title "Colección de curvas" :scatter NIL
            :title-fontsize 25
            :linewidth 1
            :annotations (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
            :padding (list 50 30 50 30)
            :xmin -30 :xmax 30 :ymin 5 :ymax 60
            :squareplot T :interpolation "CatmullRom" 
            :showXaxis NIL :showYaxis NIL :squareplot T)