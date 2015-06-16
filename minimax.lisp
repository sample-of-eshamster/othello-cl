; ab = alpha-beta

(defstruct minimax-node
  move
  score)

; Evaluation values that Nodes in the created tree have
; are calculated from the White point of view
(defun eval-game-by-minimax (game depth &key
					  (is-all-tree nil)
					  (uses-ab nil))
  (labels ((make-a-node (move)
	     (list (make-minimax-node :move move :score nil)))
	   (null-or-< (a b) (or (null a) (< a b)))
	   (null-or-> (a b) (or (null a) (> a b)))
	   (set-score (node score)
	     (if (null node)
		 score
		 (setf (minimax-node-score (get-node-value node)) score)))
	   (eval-recurse (parent rest-depth alpha beta)
	     (if (or (>= 0 rest-depth) (is-game-end game))
		 (return-from eval-recurse
		   (set-score parent (eval-game-static game *white*))))
	     (let ((best-score nil)
		   (turn (game-turn game))
		   (saves-child (or is-all-tree (eq depth rest-depth)))
		   (able-to-cut nil))
	       (do-move-store (move (make-moves game))
		 (if saves-child (setf move (clone-move move)))
		 (do-in-move-reverse game move
		   (let* ((child (if saves-child (make-a-node move)))
			  (child-score (eval-recurse child (- rest-depth 1) alpha beta)))
		     (when (or (and (eq turn *white*) (null-or-< best-score child-score))
			       (and (eq turn *black*) (null-or-> best-score child-score)))
		       (setf best-score child-score))
		     (when saves-child
		       (assert (not (null child)))
		       (insert-child parent child))
		     (when uses-ab
		       (cond ((eq turn *white*)
			      (when (null-or-< alpha child-score) (setf alpha child-score)))
			     ((eq turn *black*)
			      (when (null-or-> beta child-score) (setf beta child-score)))
			     (t (assert nil)))
		       (when (and (not (null alpha))
				  (not (null beta))
				  (> alpha beta))
			 (setf able-to-cut t)))))
		 (if (and uses-ab able-to-cut) (return)))
	       (set-score parent best-score))))
    (let ((node (make-a-node (make-a-move -1 -1))))
      (eval-recurse node depth nil nil)
      node)))

(defun eval-game-by-ab (game depth &key (is-all-tree nil))
  (eval-game-by-minimax game depth
			:is-all-tree is-all-tree
			:uses-ab t))

(defun select-move-by-minimax (game depth fn-eval)
  (minimax-node-move (get-node-value
		      (select-max-child
		       #'(lambda (node)
			   (* (minimax-node-score node)
			      (if (eq (game-turn game) *white*) 1 -1)))
		       (funcall fn-eval game depth)))))
