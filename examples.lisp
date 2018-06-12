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
(setq x (linspace 0 10))
(setq y (mapcar #'exp x))
(createplot x y :title "Exponencial"  :squareplot T)

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

;Poligon options
(setq x (first (ngon 5)))
(setq y (second (ngon 5)))
(createplot x y :title "Pentagon" :scatter NIL :strokefill "orange" :squareplot T :showXaxis NIL :showYaxis NIL :save  T :filename "Pentagon" :fileformat "jpeg")

;Circle as poligon
(setq x (first (ngon 100)))
(setq y (second (ngon 100)))
(createplot x y :title "Circle"  :scatter NIL :squareplot T :interpolation "CatmullRom")

;Hipotrochoid
(setq x (first (hipotrochoid 37 13 :d 13 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y (second (hipotrochoid 37 13 :d 13 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(createplot x y :title "Hipotrochoid"  :scatter NIL :squareplot T :interpolation "CatmullRom")

;Epitrochoid
(setq x (first (epitrochoid 37 13 :d 13 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y (second (epitrochoid 37 13 :d 13 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(createplot x y :title "Epitrochoid"  :scatter NIL :squareplot T :interpolation "CatmullRom")