;;Polygon museum
(setq x1 (list 0))
(setq y1 (list 0))

(setq x3 (first  (ngon 3 :r 2)))
(setq y3 (second (ngon 3 :r 2)))

(setq x4 (first  (ngon 4 :r 4)))
(setq y4 (second (ngon 4 :r 4)))

(setq x5 (first  (ngon 5 :r 6)))
(setq y5 (second (ngon 5 :r 6)))

(setq x6 (first  (ngon 6 :r 8)))
(setq y6 (second (ngon 6 :r 8)))

(setq x7 (first  (ngon 7 :r 10)))
(setq y7 (second (ngon 7 :r 10)))

(setq x8 (first  (ngon 8 :r 12)))
(setq y8 (second (ngon 8 :r 12)))

(setq x9 (first  (ngon 9 :r 14)))
(setq y9 (second (ngon 9 :r 14)))

(setq x10 (first  (ngon 10 :r 16)))
(setq y10 (second (ngon 10 :r 16)))

(setq x11 (first  (ngon 11 :r 18)))
(setq y11 (second (ngon 11 :r 18)))

(setq x12 (first  (ngon 12 :r 20)))
(setq y12 (second (ngon 12 :r 20)))


;Creation of epicicloid
(createplot (list x1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) 
            (list y1 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) 
            :title "Regular poligons museum" :scatter T
            :linecolor (list "#ffa190" "#ff917e" "#ff826b" "#ff7259" "#ff6347" "#e5593f" "#cc4f38" "#b24531" "#993b2a" "#7f3123" "#66271c")
            :size 4
            :title-fontsize 25
            :title-color "white"
            :linewidth 4
            :padding (list 50 30 60 50)
            :xmin -20 :xmax 20 :ymin -20 :ymax 20
            :scattercolor "white"
            :outercolor "black"
            :linecolor "red"
            :interpolation "Linear" 
            :showXaxis NIL :showYaxis NIL :squareplot T)