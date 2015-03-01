(prove:plan 8)

(load "TEST/test-util.lisp")

(prove:subtest "Test make-a-uct-node"
  (prove:ok (listp (make-a-uct-node nil)))
  (prove:ok (uct-node-p (car (make-a-uct-node (make-a-move 3 4))))))

(defun make-expanded-node (game visit-times intv)
  (let ((tree (make-a-uct-node nil))
	(param (make-uct-param :expand-intv intv)))
    (dotimes (x visit-times)
      (setf (uct-node-num (get-node-value tree)) (+ x 1))
      (setf tree (expand-child-if-needed game tree param)))
    tree))
  
(prove:subtest "Test expand-child-if-needed"
  (prove:subtest "Test num-child"
    (let* ((game (make-nth-test-game 2))
	   (num-moves (length (make-moves game))))
      (labels ((prove-child-num (intv visit-times target-num)
		 (let ((tree (make-expanded-node game visit-times intv)))
		   (prove:is (get-num-children tree) target-num))))
	(prove-child-num 3 0 0)
	(prove-child-num 3 1 0)
	(prove-child-num 3 2 1)
	(prove-child-num 3 3 1)
	(prove-child-num 3 5 2)
	(prove-child-num 3 8 3)
	(prove-child-num 3 11 4)
	(prove-child-num 3 30 num-moves))))
  (prove:subtest "Test move-valid"
    (dolist (x '(4 5))
      (let ((game (make-nth-test-game x)))
	(dolist (tree (get-children (make-expanded-node game 20 1)))
	  (let ((move (uct-node-move-from-parent (get-node-value tree))))
	    (prove:ok (check-move-valid (game-board game)
					(car move)
					(cdr move)
					(game-turn game)))))))))

(define-modify-macro test-multf (n) *)

(prove:subtest "Test select-uct-child"
  (let ((target 1)
	(ucb-coef 1.41421356))
    (labels ((test-select-uct-child (game-progress turn)
	       (let* ((game (make-nth-test-game game-progress))
		      (tree (make-expanded-node game 20 1))
		      (count 0))
		 (assert (= (game-turn game) turn))
		 (dolist (child-tree (get-children tree))
		   (setf (uct-node-num (car child-tree)) (+ count 1))
		   (setf (uct-node-sum (car child-tree))
			 (* (+ count 1) (if (= turn *white*) -1 1)))
		   (incf count 1))
		 ;; test if the node that have not been visited is selected
		 (setf (uct-node-num (car (get-nth-child target tree))) 0)
		 (prove:is (select-uct-child game tree *def-uct-param*)
			   (get-nth-child target tree)
			   :test #'equalp)
      
		 ;; test if the max uct node is selected
		 (setf (uct-node-num (car (get-nth-child target tree))) (+ target 1))
		 (test-multf (uct-node-sum (car (get-nth-child target tree))) -1)
		 (prove:is (select-uct-child game tree *def-uct-param*)
			   (get-nth-child target tree)
			   :test #'equalp))))
      (test-select-uct-child 4 *white*)
      (test-select-uct-child 5 *black*))))

(prove:subtest "Test reflect-sim-result"
  (let ((tree (make-a-uct-node nil))
	(game (init-game))
	(uct-param (make-def-uct-param)))
    (prove:is (reflect-sim-result game tree uct-param -1) -1)
    (prove:is (uct-node-num (get-node-value tree)) 1)
    (prove:is (uct-node-sum (get-node-value tree)) -1)
    
    (prove:is (reflect-sim-result game tree uct-param 9) 9)
    (prove:is (uct-node-num (get-node-value tree)) 2)
    (prove:is (uct-node-sum (get-node-value tree)) 8)))

(prove:finalize)
