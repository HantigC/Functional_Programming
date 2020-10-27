;bubble-sort problem
(defun lung_list (list)
    (do
        (
            (lung 0 (+ lung 1))
            (a list (cdr a))
        )
        ((endp a) lung)
    )
)
(defun swap (lista n)
    (setf aux (nth n lista))
    (setf (nth n lista) (nth (+ n 1) lista ))
    (setf (nth (+ n 1) lista) aux)
    lista
)
;;;
(defun bubble-sort (lista)
    (do
        (
            (i 0 (+ i 1))
        )
        ((equal i (lung_list lista)  ) lista)
        
            (do
                (
                    (j 0 (+ j 1))
                )
                ((equal j (- (lung_list lista ) 1 )) j)
                
                (if (> (nth j lista) (nth (+ j 1) lista))
                    
                    (swap lista j)
                    
                )
                
            )
        
    )

)
;;;Insertion sort
(defun insertion (lista)
    (do*
        (
            (i 1 (+ 1 i))
            (key (first lista) (nth i lista))
            (j (- i 1) (- i 1))
        )
        ((equal i (lung_list lista) ) lista)
        (do
            ( 
            )
            (
                (not(and (>= j 0) (>(nth j lista) key)))
            )
            (setf (nth (+ j 1) lista) (nth j lista))
            (setf j (- j 1))
        )
        (setf (nth (+ j 1) lista) key)
    )
)