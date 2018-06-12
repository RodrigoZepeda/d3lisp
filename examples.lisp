(load "/Users/insp/Dropbox/UNAM_POSGRADO/CALC SIMB/proyecto/lispplot/setup.lisp")

;Para evitar errores en sbcl
(defvar x NIL)
(defvar y NIL)

;Ejemplo básico de una gráfica
(setq x (list 1 2 3 4))
(setq y (list 1 -2 3 -4))
(createplot x y :title "Mi gráfica cool" :xlab "Etiqueta x" :ylab "Etiqueta y")

;Ejemplo de coseno de x
(setq x (list (* -1 pi)  (/ (* -1 pi) 2) (/ (* -1 pi) 4) (/ (* -1 pi) 8) 0 (/ pi 8) (/ pi 4) (/ pi 2) pi))
(setq y (mapcar #'sin x))
(createplot x y :title "Gráfica de seno" :xlab "x" :ylab "sin")

;Ejemplo de coseno de x
(setq x (list (* -1 pi)  (/ (* -1 pi) 2) (/ (* -1 pi) 4) (/ (* -1 pi) 8) 0 (/ pi 8) (/ pi 4) (/ pi 2) pi))
(setq y (mapcar #'sin x))
(createplot x y :title "Gráfica de seno" :xlab "$\\\\frac{x}{2}$" :ylab "sin" :interpolation "MonotoneX")

;Ejemplo de un scatter sin líneas
(setq x (make-list 500))
(setq x (mapcar (lambda (x) (random 100.0)) x))
(setq y (mapcar (lambda (x) (random 100.0)) x))
(createplot x y :title "Gráfica de seno" :xlab "x" :ylab "y" :interpolation "MonotoneX" :line NIL :size 3 :scattercolor "purple")

;Ejemplo de un polígono 
(setq x (list 5.5 4.55 4.91 6.09 6.45 5.5))
(setq y (list 4.5 5.19 6.31 6.31 5.19 4.5))
(createplot x y :title "Un pentágono" :scatter NIL :strokefill "orange" :squareplot T :showXaxis NIL :showYaxis NIL :save  T :filename "Pentagon" :fileformat "jpeg")