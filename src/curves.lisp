;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: D3 -*-
(in-package #:d3)

(defun hipotrochoid (R1 R2 &key (center (list 0 0)) (d 1) (angle-points (range 0 (* 5 pi))))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of an hipotrochoid
    https://en.wikipedia.org/wiki/Hypotrochoid
    with fixed radius R1 and rolling radius R2 centered
    at the point center with distance d from inner circle and
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - R1:       Radius of the fixed outer circle.
    - R2:       Radius of the rolling inner circle.
    - center:   Center of the outer circle in the plane.
    - d:        Diameter of the line containing 
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the hipotrochoid.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an hipotrochoid at (list (1 2)) with inner
    ;radius 2 and outer radius 4
    (hipotrochoid 4 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (list   (mapcar (lambda (theta)
                (+ (first center) (+ (* (- R1 R2) (cos theta))
                    (* d (cos (* (/ (- R1 R2) R2) theta)))))) angle-points)
            (mapcar (lambda (theta)
                (+ (second center) (+ (* (- R1 R2) (sin theta))
                    (* d (sin (* (/ (- R1 R2) R2) theta)))))) angle-points)))

(defun ngon (n &key (apothem 1) (center (list 0 0)))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of an ngon (regular
    polygon of n sides for n > 3)
    https://en.wikipedia.org/wiki/Regular_polygon
    with fixed apothem 'apothem' at the point center.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - n:        Number of sides of the polygon.
    - apothem:  Distance from the polygon's center to one 
                of its vertices
    - center:   Center of the polygon.
    
    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the vertices.

    EXAMPLES:
    --------------------------------------------------------
    ;Create a pentagon
    (ngon 5)

    --------------------------------------------------------

    "
    (let ((theta (/ (* 2 pi) n)) (x (make-list (+ n 2))) (y (make-list (+ n 2))))
        (loop for i from 0 to (1+ n)
            do (progn
                (setf (nth i x) (+ (first center)  (* apothem (cos (* i theta)))))
                (setf (nth i y) (+ (second center) (* apothem (sin (* i theta)))))))
        (list x y)))


(defun epitrochoid (R1 R2 &key (center (list 0 0)) (d 1) (angle-points (range 0 (* 5 pi))))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of an epitrochoid
    https://en.wikipedia.org/wiki/Epitrochoid
    with fixed radius R1 and rolling radius R2 centered
    at the point center with distance d from inner circle and
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - R1:       Radius of the fixed outer circle.
    - R2:       Radius of the rolling inner circle.
    - center:   Center of the outer circle in the plane.
    - d:        Diameter of the line containing 
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the epitrochoid.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an epitrochoid at (list (1 2)) with inner
    ;radius 2 and outer radius 4
    (epitrochoid 4 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (hipotrochoid R1 (* -1 R2) :center center :d d :angle-points angle-points))

(defun hypocycloid (R1 R2 &key (center (list 0 0)) (angle-points (range 0 (* 5 pi))))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of an hypocycloid
    https://en.wikipedia.org/wiki/Hypocycloid
    with fixed radius R1 and rolling radius R2 centered
    at the point center with distance d from inner circle and
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - R1:       Radius of the fixed outer circle.
    - R2:       Radius of the rolling inner circle.
    - center:   Center of the outer circle in the plane.
    - d:        Diameter of the line containing 
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the hypocycloid.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an hypocycloid at (list (1 2)) with inner
    ;radius 2 and outer radius 4
    (hypocycloid 4 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (list   (mapcar (lambda (theta)
                        (+ (first center) (+ (* (- R1 R2) (cos theta))
                          (* R2 (cos (* (/ (- R1 R2) R2) theta)))))) angle-points)
            (mapcar (lambda (theta)
                        (+ (second center) (+ (* (- R1 R2) (sin theta))
                          (* -1 R2 (sin (* (/ (- R1 R2) R2) theta)))))) angle-points)))

(defun epicycloid (R1 R2 &key (center (list 0 0)) (angle-points (range 0 (* 5 pi))))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of an epicycloid
    https://en.wikipedia.org/wiki/Epicycloid
    with fixed radius R and rolling radius r centered
    at the point center with distance d from inner circle and
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - R1:       Radius of the fixed inner circle.
    - R2:       Radius of the rolling outer circle.
    - center:   Center of the outer circle in the plane.
    - d:        Diameter of the line containing 
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the epicycloid.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an epicycloid at (list (1 2)) with inner
    ;radius 4 and outer radius 2
    (epicycloid 4 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (list   (mapcar (lambda (theta)
                        (+ (first center) (+ (* (+ R1 R2) (cos theta))
                          (* -1 R2 (cos (* (/ (+ R1 R2) R2) theta)))))) angle-points)
            (mapcar (lambda (theta)
                        (+ (second center) (+ (* (+ R1 R2) (sin theta))
                          (* -1 R2 (sin (* (/ (+ R1 R2) R2) theta)))))) angle-points)))

(defun bicorn (a &key (center (list 0 0)) (angle-points (range 0 (* 2 pi) :length-out 1000)))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of a bicorn
    https://en.wikipedia.org/wiki/Bicorn
    with fixed parameter a at the point center with calculated
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - a:       Parameter of the bicorn.
    - center:   Center of the outer circle in the plane.
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the bicorn.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create a bicorn at (list (0 0)) with parameter 1
    (bicorn 1)

    --------------------------------------------------------

    "
    (list   (mapcar (lambda (theta)
                        (+ (first center) (* a (sin theta)))) angle-points)
            (mapcar (lambda (theta)
                        (+ (second center) (* a (/ (* (cos theta) (cos theta)) 
                            (- 2 (cos theta)))))) angle-points)))

(defun involute (a &key (center (list 0 0)) (angle-points (range 0 (* 2 pi) :length-out 1000)))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of the involute of a circle 
    with parameter a
    https://en.wikipedia.org/wiki/Involute
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - a:       Parameter for the involute.
    - center:   Center of the outer circle in the plane.
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the involute.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an involute at (list (1 2)) with parameter 2
    (involute 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (list   (mapcar (lambda (theta)
                        (+ (first center) (* a (+ (cos theta) (* theta (sin theta)))))) angle-points)
            (mapcar (lambda (theta)
                        (+ (second center) (* a (- (sin theta) (* theta (cos theta)))))) angle-points)))

(defun archimedes (a &key (center (list 0 0)) (angle-points (range 0 (* 2 pi) :length-out 1000)))
    "
    DESCRIPTION:
    --------------------------------------------------------
    Function for creating vertices of Archimides' spiral
    http://mathworld.wolfram.com/ArchimedesSpiral.html
    with parameter a at the point center with 
    vertices at angle-points.
    --------------------------------------------------------

    INPUT:
    --------------------------------------------------------
    - a:        Archimidean spiral parameter.
    - center:   Center of the outer circle in the plane.
    - angle-points: Range of points of the angle at which to 
    calculate the x and y coordinates of the hipotrochoid.

    OUTPUT:
    --------------------------------------------------------
    - list: A list including the x and y coordinates
    of the different points

    EXAMPLES:
    --------------------------------------------------------
    ;Create an archimidean spiral at (list (1 2)) with 
    ;parameter 2
    (archimedes 2 :center (list 1 2))

    --------------------------------------------------------

    "
    (list  (mapcar (lambda (theta)
                        (+ (first center) (* a theta (cos theta)))) angle-points)
           (mapcar (lambda (theta)
                        (+ (second center) (* a theta (sin theta)))) angle-points)))