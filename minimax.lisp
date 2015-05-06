(defstruct minimax-node
  move
  score)

; Evaluation values that Nodes in the created tree have
; are calculated from the White point of view
(defun eval-game-by-minimax (game depth &optional (is-all-tree nil))
  (labels ((make-a-node (move)
	     (list (make-minimax-node :move move :score nil)))
	   (set-score (node score)
	     (if (null node)
		 score
		 (setf (minimax-node-score (get-node-value node)) score)))
	   (eval-recurse (parent rest-depth)
	     (if (or (>= 0 rest-depth) (is-game-end game))
		 (return-from eval-recurse
		   (set-score parent (eval-game-static game *white*))))
	     (let ((best-score nil)
		   (turn (game-turn game))
		   (saves-child (or is-all-tree (eq depth rest-depth))))
	       (dolist (move (make-moves game))
		 (do-in-move-reverse game move
		   (let* ((child (if saves-child (make-a-node move)))
			  (child-score (eval-recurse child (- rest-depth 1))))
		     (when (or (null best-score)
			       (and (eq turn *white*) (< best-score child-score))
			       (and (eq turn *black*) (> best-score child-score)))
		       (setf best-score child-score))
		     (when saves-child
		       (assert (not (null child)))
		       (insert-child parent child)))))
		 (set-score parent best-score))))
    (let ((node (make-a-node (make-a-move -1 -1))))
      (eval-recurse node depth)
      node)))
		 
(defun select-move-by-minimax (game depth fn-eval)
  (minimax-node-move (get-node-value
		      (select-max-child
		       #'(lambda (node)
			   (* (minimax-node-score node)
			      (if (eq (game-turn game) *white*) 1 -1)))
		       (funcall fn-eval game depth)))))
