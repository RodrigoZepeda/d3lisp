(defun hipotrochoid (R1 R2 &key (center (list 0 0)) (d 1) (angle-points (linspace 0 (* 5 pi))))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (+ (* (- R1 R2) (cos theta)) (* d (cos (* (/ (- R1 R2) R2) theta)))))
                    ) angle-points)
                ) 
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (+ (* (- R1 R2) (sin theta)) (* d (sin (* (/ (- R1 R2) R2) theta)))))
                    ) angle-points)
                ) 
        (list x y)
    )
)

(defun ngon (n &key (r 1) (center (list 0 0)))
"Calculates the position of ngon"
    (let ((theta (/ (* 2 pi) n)) (x (make-list (+ n 2))) (y (make-list (+ n 2))))
        (loop for i from 0 to (1+ n)
            do (progn 
                (setf (nth i x) (+ (first center) (* r (cos (* i theta)))))
                (setf (nth i y) (+ (second center) (* r (sin (* i theta)))))
            )
        )
        (list x y)
    )
)


(defun epitrochoid (R1 R2 &key (center (list 0 0)) (d 1) (angle-points (linspace 0 (* 5 pi))))
    (hipotrochoid R1 (* -1 R2) :center center :d d :angle-points angle-points)
)

(defun hipocycloid (R1 R2 &key (center (list 0 0)) (angle-points (linspace 0 (* 5 pi))))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (+ (* (- R1 R2) (cos theta)) (* R2 (cos (* (/ (- R1 R2) R2) theta)))))
                    ) angle-points)
                ) 
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (+ (* (- R1 R2) (sin theta)) (* -1 R2 (sin (* (/ (- R1 R2) R2) theta)))))
                    ) angle-points)
                ) 
        (list x y)
    )
)

(defun epicycloid (R1 R2 &key (center (list 0 0)) (angle-points (linspace 0 (* 5 pi))))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (+ (* (+ R1 R2) (cos theta)) (* -1 R2 (cos (* (/ (+ R1 R2) R2) theta)))))
                    ) angle-points)
                )
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (+ (* (+ R1 R2) (sin theta)) (* -1 R2 (sin (* (/ (+ R1 R2) R2) theta)))))
                    ) angle-points)
                )
        (list x y)
    )
)

(defun bicorn (a &key (center (list 0 0)) (angle-points (linspace 0 (* 2 pi) :lengthout 1000)))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (* a (sin theta)))
                    ) angle-points)
                )
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (* a (/ (* (cos theta) (cos theta)) (- 2 (cos theta)))))
                    ) angle-points)
                )
        (list x y)
    )
)

(defun involute (a &key (center (list 0 0)) (angle-points (linspace 0 (* 2 pi) :lengthout 1000)))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (* a (+ (cos theta) (* theta (sin theta)))))
                    ) angle-points)
                )
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (* a (- (sin theta) (* theta (cos theta)))))
                    ) angle-points)
                )
        (list x y)
    )
)

(defun archimedes (a &key (center (list 0 0)) (angle-points (linspace 0 (* 2 pi) :lengthout 1000)))
    (let (x y)
        (setf x (mapcar 
                    (lambda (theta)
                        (+ (first center) (* a theta (cos theta)))
                    ) angle-points)
                )
        (setf y (mapcar 
                    (lambda (theta)
                        (+ (second center) (* a theta (sin theta)))
                    ) angle-points)
                )
        (list x y)
    )
)
