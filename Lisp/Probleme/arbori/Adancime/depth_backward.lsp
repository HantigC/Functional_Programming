(defun depth_backward (tree)
	(cond
		((null tree) -1)
		((not (listp tree)) 0)
		((atom (first tree)) (max (+ (depth_backward (first (rest tree))) 1) (+ (depth_backward (first (rest (rest tree)))) 1) ))
	)
)

(print (depth_backward '(10 (20 40 28) (30 (27 nil 29) 59) )))
(print (depth_backward '(1 (2 2 2) (3 (3 nil nil) 3)))) 