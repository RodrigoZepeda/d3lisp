;;Creates a plot of 50 shades of gray
(let* ((n 50)
       (x (make-list n))
       (y (make-list n))
       (color (make-list n))
       (color-name (make-list n)))

      ;Loop through all RGB colors
      (let ((k -0.5) (my-step (/ 255 (1- n))) (current-step 0))
          (loop for i from 0 to (1- n)
              do (progn
                      (when (= (mod i 5) 0) (incf k))
                      (setf (nth i x) (list (+ 3 (- (mod i 5) 0.5)) (+ 3 (- (mod i 5) 0.5))
                                            (+ 3 (+ (mod i 5) 0.5)) (+ 3 (+ (mod i 5) 0.5))
                                            (+ 3 (- (mod i 5) 0.5))))
                      (setf (nth i y) (list (- k 0.5) (+ k 0.5) (+ k 0.5) (- k 0.5) (- k 0.5)))
                      (setf current-step (write-to-string (truncate (* i my-step))))
                      (setf (nth i color) (concatenate 'string
                          "rgb(" current-step "," current-step "," current-step ")"))
                      (setf (nth i color-name) (list (nth i color) (+ 3 (mod i 5)) k)))))

      (plot x y
              :title "50 Shades of Gray"
              :scatter NIL
              :line-color "none"
              :annotations-font-size 8
              :x-minimum -0.5 :x-maximum 10
              :y-minimum  0.0 :y-maximum 10.5
              :annotations color-name
              :annotations-color (reverse color)
              :show-x-axis NIL :show-y-axis NIL
              :square-plot T
              :padding (list 50 10 20 10)
              :margin  (list 40 10 10 10)
              :plot-height 1000
              :stroke-fill color))
