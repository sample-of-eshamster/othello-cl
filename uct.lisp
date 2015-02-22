(defstruct uct-node
  unexpanded-moves
  move-from-parent
  num
  sum)

(defun make-a-uct-node (move-from-parent)
  (list (make-uct-node
	 :unexpanded-moves 'has-not-made-child
	 :move-from-parent move-from-parent
	 :num 0
	 :sum 0)))

; all evaluated values are from white player's perspective

(defparameter *def-uct-param* (make-hash-table))
(setf (gethash 'intv-to-expand *def-uct-param*) 2)
(setf (gethash 'ucb-coef *def-uct-param*) 1)
(setf (gethash 'fn-make-policy *def-uct-param*) #'make-uniform-policy)

; This function is destructive about "unexpanded-moves"
(defun expand-child-if-needed (game tree expand-intv)
  (let* ((node (get-node-value tree))
	 (moves (uct-node-unexpanded-moves node)))
    (if (eq moves 'has-not-made-child)
	(progn (setf moves (make-moves game))
	       (setf (uct-node-unexpanded-moves node) moves)))
    (let ((move (car moves)))
      (if (or (null move)
	      (eq (uct-node-num node) 0)
	      (neq (mod (uct-node-num node) expand-intv) (- expand-intv 1)))
	  (return-from expand-child-if-needed tree))
      (setf (uct-node-unexpanded-moves node) (cdr moves))
      (add-child tree (make-a-uct-node move)))))
    
(defun select-uct-child (parent now-turn ucb-coef)
  (if (not (has-children parent))
      (return-from select-uct-child nil))
  (select-max-child (lambda (node)
		      (calc-ucb (uct-node-sum node)
				(uct-node-num node)
				(uct-node-num (get-node-value parent))
				:coef  ucb-coef
				:turn now-turn))
		    parent))
