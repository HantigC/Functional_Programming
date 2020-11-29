(defun depth_forward (tree &optional (depth 0) (max_depth 0))
	(cond
		((null tree) max_depth)
		((not (listp tree)) (setf max_depth (max depth max_depth)))
		((atom (first tree)) 
			(let ((_max_depth (depth_forward (first (rest tree)) (+ depth 1) max_depth)))
				(setq _max_depth (max _max_depth (+ 1 depth)))
				(depth_forward (first (rest (rest tree))) (+ depth 1) _max_depth))
			)
	)
)

(print (depth_forward '(10 (20 40 28) (30 (27 nil 29) 59) )))