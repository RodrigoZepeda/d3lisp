(defun linspace (xmin xmax &key (lengthout 100))
    (let ((x (make-list lengthout :initial-element xmin)) (n (/ (- xmax xmin) (1- lengthout))))
        (loop for i from 1 to (1- lengthout)
            do (setf (nth i x) (+ (nth (1- i) x) n))
        )
        x
    )
)