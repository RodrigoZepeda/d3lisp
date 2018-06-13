(setq x1 (first  (epitrochoid 2.17 1.13  :d 1.5 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))
(setq y1 (second (epitrochoid 2.17 1.13  :d 1.5 :angle-points (linspace 0 (* 50 pi) :lengthout 1000))))

(setq x2 (first  (ngon 100 :r 1.5)))
(setq y2 (second (ngon 100 :r 1.5)))

(setq x3 (first  (ngon 100 :r 4.8)))
(setq y3 (second (ngon 100 :r 4.8)))


(createplot (list x1 x2 x3) (list y1 y2 y3)
    :title ""
    :showXaxis NIL
    :linewidth (list 3 10 5)
    :linecolor (list "purple" "white" "#4E004E")
    :annotations (list "d3lisp" 0 -0.2)
    :annotations-fontsize 35
    :annotations-color "white"
    :squareplot T
    :scatter NIL
    :showYaxis NIL
    :save T
    :svgname "logo"
    :interpolation "Basis"
)