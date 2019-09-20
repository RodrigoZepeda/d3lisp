;;Simulates 500 points assigning each a different color
(let* ((n 500) ;;Number of points in simulation
       (x          (loop for i from 0 below n collect (list (random 1.0))))
       (y          (loop for i from 0 below n collect (list (random 1.0))))
       (point-size (loop for i from 0 below n collect (1+ (random 25))))
       (color      (loop for i from 0 below n collect
                      (concatenate 'string "rgb("
                         (write-to-string (random 255)) ","
                         (write-to-string (random 255)) ","
                         (write-to-string (random 255)) ")"))))
       (plot x y :line NIL
                 :title "Random points just floating around"
                 :x-label "Some variable"
                 :y-label "Some boring note"
                 :scatter-opacity 0.5 :scatter-color color :size point-size))
