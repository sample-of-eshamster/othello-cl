(prove:plan 0)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test game-loop"
  (labels ((test (game)
	     (game-loop game
			(player-make-mover (construct-player "test-white" 'random))
			(player-make-mover (construct-player "test-black" 'random)))
	     (prove:ok (is-game-end game))))
    (test (init-game))
    (test (make-nth-test-game 15))))

(prove:finalize)
