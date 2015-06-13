; (ql:quickload :prove)

(prove:plan 6)

(prove:subtest "Test make-a-move"
  (prove:is (make-a-move 1 2) '(1 . 2)))

(prove:subtest
    "Test set-to-move"
  (let ((move (make-a-move 1 2)))
    (prove:is (set-to-move move 3 4) (make-a-move 3 4))
    (prove:is move (make-a-move 3 4))))

(prove:subtest "Test add-move"
  (defparameter moves (list (make-a-move 1 2)))
  (prove:is (add-move moves 2 3) `(,(make-a-move 2 3) ,(make-a-move 1 2))))

(prove:subtest "Test get-nth-move"
  (defparameter moves (add-move moves 2 3))
  (prove:ok (not (get-nth-move moves -1)))
  (prove:ok (not (get-nth-move moves 10)))
  (prove:ok (not (get-nth-move nil 0)))
  (prove:is (get-nth-move moves 1) (make-a-move 1 2)))

(prove:subtest
    "Test get-fn-replace-by-next"
  (labels ((test-ok (dir expected-x expected-y)
	     (let ((move (make-a-move 4 4))
		   (expected-move (make-a-move expected-x expected-y)))
	       (multiple-value-bind (tmp-move suc)
		   (funcall (get-fn-replace-by-next dir) move)
		 (prove:is tmp-move expected-move)
		 (prove:is move expected-move)
		 (prove:ok suc))))
	   (test-ng (dir start-x start-y)
	     (let ((move (make-a-move start-x start-y)))
	       (multiple-value-bind (tmp-move suc)
		   (funcall (get-fn-replace-by-next dir) move)
		 (prove:ok (not suc)))))
	   (test (dir x1 y1 x2 y2)
	     (test-ok dir x1 y1)
	     (test-ng dir x2 y2)))
    (test *dir-up*         4 3  4 0)
    (test *dir-down*       4 5  4 7)
    (test *dir-right*      5 4  7 4)
    (test *dir-left*       3 4  0 4)
    (test *dir-left-up*    3 3  4 0)
    (test *dir-left-down*  3 5  0 4)
    (test *dir-right-up*   5 3  7 4)
    (test *dir-right-down* 5 5  4 7)))

(prove:subtest "Test other funcs"
  (prove:is (moves-len moves) 2)

  (prove:is (mapcar-moves #'(lambda (move) (car move)) moves) '(2 1)))

(prove:finalize)
