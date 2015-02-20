(prove:plan 8)

(load "TEST/test-util.lisp")

(prove:subtest "Test make-a-uct-node"
  (prove:ok (listp (make-a-uct-node nil)))
  (prove:ok (uct-node-p (car (make-a-uct-node (make-a-move 3 4))))))

(prove:subtest "Test expand-child-if-needed"
  (let* ((game (make-nth-test-game 2))
	 (num-moves (length (make-moves game))))
    (labels ((prove-child-num (intv visit-times target-num)
	       (let ((tree (make-a-uct-node nil)))
		 (dotimes (x visit-times)
		   (setf (uct-node-num (get-node-value tree)) (+ x 1))
		   (setf tree (expand-child-if-needed game tree intv)))
		 (prove:is (get-num-children tree) target-num))))
      (prove-child-num 3 0 0)
      (prove-child-num 3 1 0)
      (prove-child-num 3 2 1)
      (prove-child-num 3 3 1)
      (prove-child-num 3 5 2)
      (prove-child-num 3 8 3)
      (prove-child-num 3 11 4)
      (prove-child-num 3 20 4))))

(prove:finalize)
