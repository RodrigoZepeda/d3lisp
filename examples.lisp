

;Para evitar errores en sbcl
(defvar x NIL)
(defvar y NIL)

;Ejemplo básico de una gráfica
(setq x (list 1 2 3 4))
(setq y (list 1 -2 3 -4))
(createplot x y :title "Mi gráfica cool" :xlab "Etiqueta x" :ylab "Etiqueta y")

;Ejemplo de uso de linspace
(setq x (linspace 2 10))
(setq y (mapcar #'exp x))
(createplot x y :title "Exponencial"  :squareplot T :plotheight 500 :plotwidth 500)

;Gráfica doble
(setq x (list (linspace 0 10) (linspace 0 10)))
(setq y (list (mapcar #'sin (first x)) (mapcar #'cos (second x))))
(createplot x y :title "My favourite trigonometric functions"  :squareplot NIL :scatter (list NIL NIL) 
                :linecolor (list "red" "blue") :annotations (list (list "cos" pi -1) (list "sin" (/ pi 2) 1)))

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
