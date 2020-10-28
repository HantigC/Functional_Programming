;; Totoian Marius-Horatiu
;; Operatii pe arbori binari de cautare

;; Cautarea unei valori intr-un arbore binar
(defun searchBTree (bTree value)
   (COND 
       ((NULL bTree) NIL)
       (T (COND
           ((> (CAR bTRee) value) (searchBTree (CADR bTree) value))
           ((< (CAR bTRee) value) (searchBTree (CADDR bTree) value))
           (T value)
          )   
       )
   )
)

;; Retruneaza o lista care reprezinta arborele bTree inserat cu value
;; Observatie : bTree ramane nemodificat
(defun insertBTree (bTree value)
   (COND 
       ((NULL bTree) (LIST value))
       (T (COND
             ((> (CAR bTRee) value) (list (CAR bTree) (insertBTree (CADR bTree) value) (CADDR bTree)))
             ((< (CAR bTRee) value) (list (CAR bTree) (CADR bTree) (insertBTree (CADDR bTree) value)))
             (T bTree)   ;; este deja inserat
           ) 
       )   
   )
)

;; Insereaza in bTree valoarea value. bTree se modifica
(defun insertBTree_destr (bTree value)
   (COND 
       ((NULL bTree) (LIST value))
       (T (COND
             ((> (CAR bTree) value) (CONS (CAR bTree) (setf (CDR bTree) (LIST (insertBTree_destr (CADR bTree) value) (CADDR bTree) ))))
             ((< (CAR bTree) value) (CONS (CAR bTree) (setf (CDR bTree) (LIST (CADR bTree) (insertBTree_destr (CADDR bTree) value) )))) 
             (T bTRee)
          )   
       )
   )
)

;; Minimul dintr-un arbore
(defun minim_of_Tree (bTree)
   (COND 
      ((NULL (CADR bTree)) (CAR bTree))
      (T (minim_of_Tree (CADR bTree)))
   )
)

;; Retruneaza o lista care reprezinta arborele bTree fara value
;; bTree ramane nemodificat
(defun deleteBTree (bTree value)
   (COND 
      ((NULL bTree) bTree)
      ((> (CAR bTree) value) (LIST (CAR bTree) (deleteBTree (CADR bTree) value) (CADDR bTree) ))
      ((< (CAR bTree) value)  (LIST (CAR bTree) (CADR bTree) (deleteBTree (CADDR bTree) value) ))
      (T  (COND 
             ((NULL (CADR bTree)) (CADDR bTree))
             ((NULL (CADDR bTree)) (CADR bTree))
             (T (LIST (minim_of_Tree (CADDR bTree)) (CADR bTree) (deleteBTree (CADDR bTree) (minim_of_Tree (CADDR bTree))) ) )
       
          ) 
        
      )
   
   )
)

;; Sterge din arborele bTree value
;; bTree se modifica
(defun deleteBTree_destr (bTree value)
   (COND 
      ((NULL bTree) bTree)
      ((> (CAR bTree) value) (CONS (CAR bTree) (setf (CDR bTree) (LIST (deleteBTree_destr (CADR bTree) value) (CADDR bTree) ))))
      ((< (CAR bTree) value) (CONS (CAR bTree) (setf (CDR bTree) (LIST (CADR bTree) (deleteBTree_destr (CADDR bTree) value) ))))
      (T  (COND 
             ((NULL (CADR bTree)) (CADDR bTree))
             ((NULL (CADDR bTree)) (CADR bTree))
             (T (CONS (setf (CAR bTRee) (minim_of_Tree (CADDR bTree)))  ;; sterg valoarea
                      (setf (CDR bTree) (LIST (CADR bTree) (deleteBTree_destr (CADDR bTree) (minim_of_Tree (CADDR bTree))))))) 
       
          ) 
        
      )
   
   )
)

        ;;       20
        ;;      /   \
        ;;     11    25
        ;;    / \   /  \
        ;;   9  13 24  30
        ;;     /  \
        ;;    12  14 
(setq arb1 '(20 (11 (9) (13 (12) (14))) (25 (24) (30))))

        ;;         9
        ;;       /  \
        ;;      5   12
        ;;     / \  /
        ;;    2  8 10
        ;;      /
        ;;     7  
(setq arb2 '(9 (5 (2) (8 (7))) (12 (10))))

(print "Testarea Cautarii :")
(print (searchBTree arb1 25))
(print (searchBTree arb1 3))
(print (searchBTree arb2 9))
(print (searchBTree arb2 7))
(print (searchBTree arb2 999))

(print "Testarea Inserarii :")
(print (insertBTree arb1 8))
(print (insertBTree arb1 15))
(print "arb1 =")
(print arb1)
(print (insertBTree_destr arb1 15))
(print "arb1 =")
(print arb1)
(print (insertBTree arb2 8))
(print (insertBTree_destr arb2 11))
(print "arb2 =")
(print arb2)

(print "Testarea Stergerii :")
(print (deleteBTree arb1 8))
(print (deleteBTree arb1 11))
(print (deleteBTree arb1 20))
(print "arb1 =")
(print arb1)
(print (deleteBTree_destr arb1 20))
(print "arb1 =")
(print arb1)
(print (deleteBTree arb2 7))
(print (deleteBTree_destr arb2 7))
(print "arb2 =")
(print arb2)

