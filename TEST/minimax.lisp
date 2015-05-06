(prove:plan 2)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test eval-game-by-minimax"
  (prove:subtest
      "Check selection"
    (labels ((get-score (node)
	       (minimax-node-score (get-node-value node)))
	     (prove-selection (game node)
	       (prove:is (get-score node)
			 (get-score
			  (select-max-child
			   #'(lambda (child)
			       (* (minimax-node-score child)
				  (if (= (game-turn game) *white*) 1 -1)))
			   node))))   
	     (test-at (start-depth)
	       (let* ((depth 4)
		      (game (make-nth-test-game start-depth))
		      (first-game-depth (get-game-depth game))
		      (eval-tree (eval-game-by-minimax game depth t)))
		 (prove-selection game eval-tree)
		 (prove:is (get-tree-depth eval-tree) depth)
		 (prove:is (get-game-depth game) first-game-depth)
		 (do-children (child eval-tree)
		   (do-in-move-reverse
		       game
		       (minimax-node-move (get-node-value child))
		     (prove-selection game child))))))
      (test-at 4)
      (test-at 5)))

  (prove:subtest
      "Check near the game end"
    (let* ((depth 6)
	   (game (make-nth-test-game 58))
	   (eval-tree (eval-game-by-minimax game depth t)))
      (assert (not (is-game-end game)))
      (prove:ok (< (get-tree-depth eval-tree) depth)))))

(prove:subtest
    "Test select-move-by-minimax"
  (labels ((test-func (fn)
	     (let ((depth 4))
	       (dolist (start-depth '(4 5))
		 (let* ((game (make-nth-test-game start-depth))
			(move (select-move-by-minimax game depth fn)))
		   (prove:ok (check-move-valid (game-board game)
					       (car move) (cdr move)
					       (game-turn game))))))))
    (test-func #'eval-game-by-minimax)))

(prove:finalize)
