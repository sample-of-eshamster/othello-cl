(prove:plan 1)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test get-nth-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    
    (prove:ok (not (get-nth-move store -1)))
    (prove:ok (not (get-nth-move store 10)))
    (prove:ok (not (get-nth-move nil 0)))
    (prove:is (get-nth-move store 1) (make-a-move 3 4))))

(prove:subtest
    "Test mapcar-move-store"
  (let* ((store (make-moves (make-nth-test-game 5)))
	 (move-list (mapcar-move-store #'(lambda (move) move) store)))
    (prove:is (move-store-count store) (length move-list))
    (dolist (move move-list)
      (prove:ok (contains-move store (move-x move) (move-y move))))))

(prove:finalize)
