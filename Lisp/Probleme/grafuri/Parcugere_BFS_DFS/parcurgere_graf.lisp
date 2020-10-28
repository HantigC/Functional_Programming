;; Totoian Marius-Horatiu
;; Parcurgere de grafuri

; o coada goala va fii de forma (NIL)
(defun isEmpty (ls)
    (equal ls '(NIL))
)

; Adauga elementul in coada negoala
(defun push_queue (ls element)
    (setf (CDR ls) (APPEND (CDR ls) (LIST element)))
    ls
)

(defun pop_queue (ls)
    (LET ((rez ))
       (setf rez (CAR ls))
       (setf (CAR ls) (CADR ls))  
       (setf (CDR ls) (CDDR ls))  
       rez
    )
)

; Verifica daca element este in ls. Daca se afla in ls, se va returna
; chiar element, altfel se va returna NIL
(defun contains (ls element)
     (COND
        ((null ls) nil)
        ((equal (CAR ls) element) element)
        (T (contains (CDR ls) element))
     )
)

; Returneaza lista de vecini a nodului
(defun get_neighbours (ls nod)
    (COND 
       ((equal (CAAR ls) nod) (CADAR ls))
       (T (get_neighbours (CDR ls) nod))
    )
)

(defun bfs (ls nod)
   (DO 
      (
       (visited (list nod)) 
       (queue (list nod))
       (bfs_list (list nod))
       (currentNode nod)
      )
      
      ((isEmpty queue) bfs_list)

      (setf currentNode (pop_queue queue))
      (if 
         ; sa nu pun nod de doua ori in bfs_list
         (not (equal currentNode nod))
         (push_queue bfs_list currentNode)
      )

      (do 
         (
             (iter (get_neighbours ls currentNode) (cdr iter))
         )
         ((endp iter) )
         (if 
            (not (equal (contains visited (CAR iter)) (CAR iter)))
            (funcall (lambda (x) 
                     (push_queue visited x)
                     (if 
                        ; daca coada este goala creez o coada doar cu un element
                        (isEmpty queue)
                        (setf queue (LIST x))
                        (push_queue queue x)      

                     )   
                     )
                     (CAR iter)
            )
            )
        )
   )
)

(defun dfs (ls nod)
    (setf visited (LIST nod))
    (dfs_util ls nod visited)
    visited
)

(defun dfs_util (ls nod visited)
   (if
      ; Nu adaug nodul de start de 2 ori
      (not (contains visited nod))
      (push_queue visited nod)
   )

   (do 
         (
             (iter (get_neighbours ls nod) (cdr iter))
         )
         ((endp iter) )
         (if 
            (not (equal (contains visited (CAR iter)) (CAR iter)))
            (dfs_util ls (CAR iter) visited)
            
         )
    )
)

(setq graph1 '( (1 (4 5 7)) (2 (4 6)) (3 (4 5 8)) (4 (1 2 3)) (5 (3 6)) (6 (2 5)) (7 (1 8)) (8 (3 7)) ))
(setq graph2 '( (1 (2 3 4 5)) (2 (4)) (3 (1 5)) (4 (1 2 5)) (5 (1 3 4)) ))
(setq graph3 '( (1 (3 5)) (2 (3 4 5 6)) (3 (1 2 6)) (4 (2 5)) (5 (1 2 4)) (6 (2 3)) ))   

(print (get_neighbours graph1  7))
(print (get_neighbours graph2  2))
(print (get_neighbours graph3  5))
(print "BFS")
(print (bfs graph1 1))
(print (bfs graph1 5))
(print (bfs graph2 1))
(print (bfs graph3 1))
(print "DFS")
(print (dfs graph1 1))
(print (dfs graph1 5))
(print (dfs graph2 1))
(print (dfs graph3 1))