(defun hipotrochoid (R1 R2 &key (d 1) (angle-points (linspace 0 (* 5 pi))))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (* (- R1 R2) (cos theta)) (* d (cos (* (/ (- R1 R2) R2) theta))))
                    ) angle-points)
                ) 
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (* (- R1 R2) (sin theta)) (* d (sin (* (/ (- R1 R2) R2) theta))))
                    ) angle-points)
                ) 
        (list x y)
    )
)

(defun epitrochoid (R1 R2 &key (d 1) (angle-points (linspace 0 (* 5 pi))))
    (hipotrochoid R1 (* -1 R2) :d d :angle-points angle-points)
)