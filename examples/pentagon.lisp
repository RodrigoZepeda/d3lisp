;Regular ngon
(setq x (first (ngon 5)))
(setq y (second (ngon 5)))
(createplot x y :title "Pentagon" :squareplot T :strokefill "#47476b" :linecolor "black"
                :padding (list 75 75 60 60)
                :title-fontsize 50
                :scatter NIL :squareplot T :showXaxis NIL :showYaxis NIL 
                :save T :svgname "pentagon")