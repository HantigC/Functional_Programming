 (defun LEN (lst)
    (cond ((null lst) 0)
        (T (+ (LEN (rest lst)) 1))
    )
)
 
 (defun mergeFunct (leftL rightL)
    (cond
      ((= (LEN leftL) 0) rightL)
      
      ((= (LEN rightL) 0) leftL)
      
      ((< (car leftL) (car rightL)) 
      		(append (list (car leftL)) (mergeFunct (cdr leftL) rightL)))
      		
      ((= (car leftL) (car rightL)) 
      		(append (list (car leftL) (car rightL)) (mergeFunct (cdr leftL) (cdr rightL))))
      		
      ((> (car leftL) (car rightL)) 
      		(append (list (car rightL)) (mergeFunct leftL (cdr rightL))))
    )
  )

(defun mergeSort (lst)
  (let ((length (LEN lst)))
    (cond
      ((= length 1) lst)
      (t (mergeFunct (mergeSort (subseq lst 0 (ceiling (/ length 2))))
      		      ;subseq returneaza toate elementele intre indicii 0 si lenght/2
                     (mergeSort (subseq lst (ceiling (/ length 2)))))
                     ;cu un singur parametru returneaza toate elementele de dupa indicele 			     ;leght/2 pana la nil 
                     ;ceiling returneaza cel mai mic numar intreg mai mare defcat length/2
                     
      )
    )
  )
)
(print "1 2 3 4 5 6 7 8 9 =>")
(print  (mergeSort '(1 2 3 4 5 6 7 8 9)))
(FRESH-LINE)
(print "9 8 7 6 5 4 3 2 1 =>")
(print  (mergeSort '(9 8 7 6 5 4 3 2 1)))
(FRESH-LINE)
(print "3 7 4 32 43 95 102 256 478 34 56 26 753 54 2 1 =>")
(print  (mergeSort '(3 7 4 32 43 95 102 256 478 34 56 26 753 54 2 1)))
(FRESH-LINE)


