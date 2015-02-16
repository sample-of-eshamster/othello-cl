(load "TEST/test-util.lisp")

(defparameter *test-eval-param* (make-hash-table))
(setf (gethash 'corner *test-eval-param*) 400)
(setf (gethash 'num-move *test-eval-param*) 100)

(prove:plan 2)

(prove:subtest "Test eval without corner"
  (prove:is (eval-game-static (make-nth-test-game 3) *white*) -2)
  (prove:is (eval-game-static (make-nth-test-game 3) *black*) 2)

  (prove:is (eval-game-static (make-nth-test-game 4) *white*) 9)
  (prove:is (eval-game-static (make-nth-test-game 4) *black*) -9))

(prove:subtest "Test eval including corner"
  (prove:is (eval-game-static (make-nth-test-game 55) *white*) -1)
  (prove:is (eval-game-static (make-nth-test-game 55) *black*) 1)
  (prove:is (eval-game-static (make-nth-test-game 55) *white*
			      *test-eval-param*) -100))

(prove:finalize)
