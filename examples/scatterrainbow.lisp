;;Simulates 1000 points assigning each a different color
(setf n 2000) ;;Number of points in simulation
(setf x     (mapcar (lambda (x) (list (1- (random 2.0)))) (make-list n)))
(setf y     (mapcar (lambda (x) (list (1- (random 2.0)))) (make-list n)))
(setf color 
    (mapcar 
        (lambda (x) 
            (concatenate 'string "rgb(" 
                        (write-to-string (random 255)) "," 
                        (write-to-string (random 255)) "," 
                        (write-to-string (random 255)) ")"
            )
        ) 
        (make-list n)
    )
)
(setf sizes (mapcar (lambda (x) (1+ (random 10))) (make-list n)))
(createplot x y :line NIL 
        :title "Random points just floating around"
        :xlab "Some variable"
        :ylab "Some boring note"
        :scatteropacity 0.5 :scattercolor color :size sizes)

