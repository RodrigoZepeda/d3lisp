(defun ngon (n)
"Calculates the position of ngon"
    (let ((theta (/ (* 2 pi) n)) (x (make-list (+ n 2))) (y (make-list (+ n 2))))
        (loop for i from 0 to (1+ n)
            do (progn 
                (setf (nth i x) (cos (* i theta)))
                (setf (nth i y) (sin (* i theta)))
            )
        )
        (list x y)
    )
)
