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

(defstruct uct-param
  expand-intv
  ucb-coef
  fn-make-policy)

(defun make-def-uct-param ()
  (make-uct-param :expand-intv 2
		  :ucb-coef 1
		  :fn-make-policy #'make-uniform-policy))
  
(defparameter *def-uct-param* (make-def-uct-param))

; This function is destructive
(defun expand-child-if-needed (game tree uct-param)
  (let* ((node (get-node-value tree))
	 (moves (uct-node-unexpanded-moves node))
	 (expand-intv (uct-param-expand-intv uct-param)))
    (if (eq moves 'has-not-made-child)
	(progn (setf moves (make-moves game))
	       (setf (uct-node-unexpanded-moves node) moves)))
    (let ((move (car moves)))
      (if (or (null move)
	      (eq (uct-node-num node) 0)
	      (neq (mod (uct-node-num node) expand-intv) (- expand-intv 1)))
	  (return-from expand-child-if-needed tree))
      (setf (uct-node-unexpanded-moves node) (cdr moves))
      (insert-child tree (make-a-uct-node move)))))
    
(defun select-uct-child (game parent uct-param)
  (if (not (has-children parent))
      (return-from select-uct-child nil))
  (let ((ucb-coef (uct-param-ucb-coef uct-param))
	(now-turn (game-turn game)))
    (select-max-child (lambda (node)
			(calc-ucb (uct-node-sum node)
				  (uct-node-num node)
				  (uct-node-num (get-node-value parent))
				  :coef  ucb-coef
				  :turn now-turn))
		      parent)))

(defun reflect-sim-result (game uct-tree uct-param result)
  (incf (uct-node-sum (get-node-value uct-tree)) result)
  (incf (uct-node-num (get-node-value uct-tree)))
  (values result uct-tree))

(defun mcts-simulate-once (game tree param &key
					     (fn-select #'select-uct-child)
					     (fn-expand #'expand-child-if-needed)
					     (fn-simulate #'mc-simulate-once)
					     (fn-backprop #'reflect-sim-result))
  (labels ((sim-once (node)
	     (funcall fn-expand game node param)
	     (let ((child-tree (funcall fn-select game node param)))
	       (funcall fn-backprop
			game
			node
			param
			(if (null child-tree)
			    (funcall fn-simulate game (uct-param-fn-make-policy param))
			    (do-in-move-reverse
				game
				(uct-node-move-from-parent (get-node-value child-tree))
			      (sim-once child-tree)))))))
    (cadr (multiple-value-list (sim-once tree)))))
