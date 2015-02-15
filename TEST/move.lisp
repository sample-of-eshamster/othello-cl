; (ql:quickload :prove)

(prove:plan 4)

(prove:subtest "Test make-a-move"
  (prove:is (make-a-move 1 2) '(1 . 2)))

(prove:subtest "Test add-move"
  (defparameter moves (list (make-a-move 1 2)))
  (prove:is (add-move moves 2 3) `(,(make-a-move 2 3) ,(make-a-move 1 2))))

(prove:subtest "Test get-nth-move"
  (defparameter moves (add-move moves 2 3))
  (prove:ok (not (get-nth-move moves -1)))
  (prove:ok (not (get-nth-move moves 10)))
  (prove:ok (not (get-nth-move nil 0)))
  (prove:is (get-nth-move moves 1) (make-a-move 1 2)))

(prove:subtest "Test other funcs"
  (prove:is (moves-len moves) 2)

  (prove:is (mapcar-moves #'(lambda (move) (car move)) moves) '(2 1)))

(prove:finalize)
