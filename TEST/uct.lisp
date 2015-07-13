(prove:plan 7)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test make-a-uct-node"
  (prove:ok (listp (make-a-uct-node nil)))
  (prove:ok (uct-node-p (car (make-a-uct-node (make-a-move 3 4))))))

(defun make-expanded-node (game visit-times intv)
  (let ((tree (make-a-uct-node nil))
	(param (make-uct-param :expand-intv intv)))
    (dotimes (x visit-times)
      (setf (uct-node-num (get-node-value tree)) (+ x 1))
      (setf tree (expand-child-if-needed game tree param)))
    tree))

(prove:subtest
    "Test expand-child-if-needed"
  (prove:subtest
      "Test num-child"
    (let* ((game (make-nth-test-game 2))
	   (num-moves (move-store-count (make-moves game))))
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
  (prove:subtest
      "Test move-valid"
    (dolist (x '(4 5))
      (let ((game (make-nth-test-game x)))
	(dolist (tree (get-children (make-expanded-node game 20 1)))
	  (let ((move (uct-node-move-from-parent (get-node-value tree))))
	    (prove:ok (check-move-valid (game-board game)
					(car move)
					(cdr move)
					(game-turn game)))))))))

(define-modify-macro t-multf (n) *)
(defmacro t-nth-uct-val (n tree)
  `(get-node-value (get-nth-child ,n ,tree)))

(prove:subtest
    "Test select-uct-child"
  (let ((target 1))
    (labels ((test-select-uct-child (game-progress)
	       (let* ((game (make-nth-test-game game-progress))
		      (turn (game-turn game))
		      (tree (make-expanded-node game 20 1)))
		 (do-children (child-tree tree)
		   (setf (uct-node-num (car child-tree)) 10)
		   (setf (uct-node-sum (car child-tree))
			 (* 10 (if (= turn *white*) -1 1))))
		 (prove:subtest
		     "test if the node that have not been visited is selected"
		   (setf (uct-node-num (t-nth-uct-val target tree)) 0)
		   (prove:is (select-uct-child game tree *def-uct-param*)
			     (get-nth-child target tree)
			     :test #'equalp))
		 
		 (prove:subtest
		     "test if the max uct node is selected"
		   (setf (uct-node-num (t-nth-uct-val target tree)) 10)
		   (t-multf (uct-node-sum (t-nth-uct-val target tree)) -1)
		   (print-tree tree
			       :max-depth 2
			       :f-proc-value #'(lambda (node)
						 (format nil "num: ~A, sum: ~A"
							 (uct-node-num node)
							 (uct-node-sum node))))
		   (prove:is (select-uct-child game tree *def-uct-param*)
			     (get-nth-child target tree)
			     :test #'equalp)))))
      (test-select-uct-child 4)
      (test-select-uct-child 5))))

(prove:subtest
    "Test select-uct-node-by-ave"
  (let* ((game (make-nth-test-game 4))
	 (tree (make-expanded-node game 20 1))
	 (child-num (get-num-children tree))
	 (count 1))
    (assert (> child-num 2))
    
    (do-children (child tree)
      (setf (uct-node-num (get-node-value child)) 10)
      (setf (uct-node-sum (get-node-value child)) count)
      (incf count 1))

    (labels ((prove-select (turn target-idx)
	       (prove:is (select-uct-node-by-ave turn tree)
			 (get-nth-child target-idx tree)
			 :test #'equalp)))
      (prove:subtest
	  "Test if the max node is selected"
	(prove-select *white* (- child-num 1))
	(prove-select *black* 0))
      
      (prove:subtest
	  "Test if the node that have not been visited is not selected"
	(setf (uct-node-num (t-nth-uct-val (- child-num 1) tree)) 0)
	(prove-select *white* (- child-num 2))
	(setf (uct-node-num (t-nth-uct-val 0 tree)) 0)
	(prove-select *black* 1)))))

(prove:subtest
    "Test reflect-sim-result"
  (let ((tree (make-a-uct-node nil))
	(game (init-game))
	(uct-param (make-def-uct-param)))
    (prove:is (reflect-sim-result game tree uct-param -1) -1)
    (prove:is (uct-node-num (get-node-value tree)) 1)
    (prove:is (uct-node-sum (get-node-value tree)) -1)
    
    (prove:is (reflect-sim-result game tree uct-param 9) 9)
    (prove:is (uct-node-num (get-node-value tree)) 2)
    (prove:is (uct-node-sum (get-node-value tree)) 8)))

(prove:subtest
    "Test mcts-simulate-once"
  (defparameter *test-game* (make-nth-test-game 4))
  (defparameter *uct-node* (make-a-uct-node nil))
  (defparameter *uct-param* (make-def-uct-param))
  (setf (uct-param-expand-intv *uct-param*) 2)
  (dotimes (x 100)
    (setf *uct-node* (mcts-simulate-once *test-game* *uct-node* *uct-param*)))
  (prove:ok (> (get-tree-size *uct-node*) 5)) ; 5 itself is meaningless
  (prove:ok (> (get-tree-depth *uct-node*) 1))
  (print-tree *uct-node*
	      :max-depth 2
	      :f-proc-value #'(lambda (node)
				(format nil "num: ~A, sum: ~A"
					(uct-node-num node)
					(uct-node-sum node)))))

(prove:subtest
    "Test uct-simulate"
  (labels ((test-simulate (start-depth times)
	     (let* ((game (make-nth-test-game start-depth))
		    (move (uct-simulate game (make-def-uct-param) times)))
	       (prove:ok (check-move-valid (game-board game) (car move) (cdr move) (game-turn game))))))
    (test-simulate 12 50)
    (test-simulate 13 50)))


(prove:finalize)
