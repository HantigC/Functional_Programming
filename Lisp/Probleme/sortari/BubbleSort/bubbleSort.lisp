
(defun bubble(lista)
    (loop repeat (- (length lista) 1) do
        (loop for ls on lista while (rest ls) do
            (when (> (car ls) (cadr ls))
                (rotatef (car ls) (cadr ls))
            )
            
        )
    )
  lista
)

(defun bubble_rec (lista n)
    (cond
        ((= n 1) nil)
        (T
            (loop for i from 0 to (- n 1) do
                (when (> (nth i lista) (nth (- n 1) lista))
                    (rotatef (nth i lista) (nth (- n 1) lista))     
                )
                
            )
            
        (bubble_rec lista (- n 1))    
        )
        
    )
    lista    
)


