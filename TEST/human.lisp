(prove:plan 1)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test eval-play-command"
  (labels ((test-ok (com expected &optional game)
	     (when (null game) (setf game (make-nth-test-game 6)))
	     (let ((result (eval-play-command game com)))
	       (if expected (prove:ok result) (prove:ok (not result))))))
    (prove:subtest
	"Test print command"
      (test-ok '(print) nil))
    (prove:subtest
	"Test move command"
      (test-ok '(move) nil)
      (test-ok '(move 1) nil)
      (test-ok '(move -1 1) nil)
      (test-ok '(move "ab" 1) nil)
      (let* ((game (make-nth-test-game 11))
	     (move (get-nth-move (make-moves game) 0)))
	(test-ok (list 'move (car move) (cdr move)) t game)
	(prove:is (get-game-depth game) 12)
	; We doesn't elaborate the search for an invalid move
	(assert (not (check-move-valid (game-board game) 0 0 (game-turn game))))
	(test-ok '(move 0 0) nil)))
    (prove:subtest
	"Test reverse command"
      (let ((game (make-nth-test-game 6)))
	(test-ok '(reverse) t game)
	(prove:is (get-game-depth game) 5)
	(test-ok '(reverse) nil (init-game))))

    (prove:subtest
	"Test not-defined command"
      (test-ok '(not-defined com) nil))))

(prove:finalize)
