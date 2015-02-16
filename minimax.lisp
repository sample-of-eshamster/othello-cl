(defstruct minimax-node
  move
  score)

(defun eval-game-by-minimax (game depth &optional (is-all-tree nil))
  (labels ((make-a-node (value move)
	     (list (list value move)))
	   (eval-recurse (move-from rest-depth)
	     (if (or (>= 0 rest-depth) (is-game-end game))
		 (return-from eval-recurse (make-a-node
					    (eval-game-static game *white*) move-from)))
	     (let ((node (make-a-node nil move-from))
		   (turn (game-turn game)))
	       (dolist (move (make-moves game))
		 (move-game game (car move) (cdr move))
		 (let* ((child (eval-recurse move (- rest-depth 1)))
			(value (caar child)))
		   (cond ((null (caar node)) (setf (caar node) value))
			 ((and (eq turn *white*) (< (caar node) value))
			  (setf (caar node) value))
			 ((and (eq turn *black*) (> (caar node) value))
			  (setf (caar node) value)))
		   (if (or is-all-tree (eq depth rest-depth))
		       (setf node (add-child node child))))
		 (reverse-game game))
	       node)))
    (eval-recurse (make-a-move -1 -1) depth)))
		 
(defun select-move-by-minimax (game depth fn-eval)
  (cadar (select-max-child #'(lambda (node)
			       (* (car node)
				  (if (eq (game-turn game) *white*) 1 -1)))
			   (funcall fn-eval game depth))))

