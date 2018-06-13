;Simplest example
(setq x (list 10 15 30 50))
(setq y (list 1 2 3 4))
(createplot x y :title "Awesome plot" 
                :xlab "Time since I started using LISP" 
                :ylab "Number of extra parentheses in my life")
