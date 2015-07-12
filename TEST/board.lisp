(prove:plan 8)

(prove:subtest "Test init-board"
  (prove:is (init-board)
	    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    :test #'equalp))

(prove:subtest "Test is-in-board"
  (prove:ok (is-in-board 0 0))
  (prove:ok (is-in-board 7 7))
  (prove:ok (is-in-board 5 3))
  (prove:ok (not (is-in-board 5 -1)))
  (prove:ok (not (is-in-board -1 3)))
  (prove:ok (not (is-in-board 4 8)))
  (prove:ok (not (is-in-board 8 4)))
  (prove:ok (not (is-in-board nil 4)))
  (prove:ok (not (is-in-board 3 nil))))

(prove:subtest "Test get-next-cell"
  (prove:is (get-next-cell 1 2 *dir-down*)  '(1 . 3))
  (prove:is (get-next-cell 1 2 *dir-up*)    '(1 . 1))
  (prove:is (get-next-cell 1 2 *dir-right*) '(2 . 2))
  (prove:is (get-next-cell 1 2 *dir-left*)  '(0 . 2))
  (prove:is (get-next-cell 1 2 *dir-right-up*)   '(2 . 1))
  (prove:is (get-next-cell 1 2 *dir-right-down*) '(2 . 3))
  (prove:is (get-next-cell 1 2 *dir-left-up*)    '(0 . 1))
  (prove:is (get-next-cell 1 2 *dir-left-down*)  '(0 . 3))
  (prove:ok (not (get-next-cell 7 2 *dir-right*))))

(defparameter *test-board* (init-board))

(prove:subtest "Test get-piece"
  (prove:is (get-piece *test-board* 3 3) *black*)
  (prove:is (get-piece *test-board* 4 3) *white*)
  (prove:ok (is-empty (get-piece *test-board* 0 3)))
  (prove:ok (null (get-piece *test-board* -1 5))))

(prove:subtest "Test make-moves-on-board"
  (labels ((test (store expected-moves)
	     (prove:is (move-store-count store) (length expected-moves))
	     (dolist (move expected-moves)
	       (prove:ok (contains-move store (car move) (cdr move))))))
  (test (make-moves-on-board *test-board* *white*)
    '((4 . 5) (5 . 4) (2 . 3) (3 . 2)))
  (test (make-moves-on-board *test-board* *black*)
    '((3 . 5) (2 . 4) (5 . 3) (4 . 2)))
  (test (make-moves-on-board *test-board* nil) nil)))

(prove:subtest "Test move-on-board"
  (prove:subtest "Test error"
    (prove:ok (not (move-on-board *test-board* -1 5 *white*)))
    (prove:ok (not (move-on-board *test-board* 3 5 nil)))
    (prove:ok (not (move-on-board *test-board* 5 5 *white*))))

  (prove:subtest "Test move"
    (prove:is *test-board* (init-board) :test #'equalp)
    (labels ((prove-reverse-list (result expected)
	       (prove:is-type result 'move-store)
	       (prove:is (move-store-count result) (length expected))
	       (let ((all t))
		 (dolist (move expected)
		   (unless (contains-move result (car move) (cdr move))
		     (setf all nil)
		     (return)))
		 (prove:ok all "All of moves in the list are contained in the reverse result"))))
	     (prove-reverse-list (move-on-board *test-board* 4 5 *white*) '((4 . 4)))
	     (prove:is *test-board*
	       #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :test #'equalp)
	     (move-on-board *test-board* 3 5 *black*)
	     (move-on-board *test-board* 2 4 *white*)
	     (prove-reverse-list (move-on-board *test-board* 5 5 *black*) '((4 . 4) (4 . 5)))
	     (prove:is *test-board*
	       #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 -1 1 -1 0 0 0 0 0 1 -1 -1 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	       :test #'equalp))))

(prove:subtest "Test count-piece"
  (prove:is (count-piece *test-board* *white*) 3)
  (prove:is (count-piece *test-board* *black*) 5)
  (prove:ok (not (count-piece *test-board* nil)))
  (prove:ok (not (count-piece '(1 2) *white*))))

(prove:subtest "Test print-board"
  (prove:is-print (print-board *test-board*)
		  "   01234567
|0 -------- |
|1 -------- |
|2 -------- |
|3 ---XO--- |
|4 --OOX--- |
|5 ---XXX-- |
|6 -------- |
|7 -------- |
"))

(prove:finalize)
