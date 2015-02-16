(prove:plan 3)

(load "TEST/test-util.lisp")

(prove:subtest "Test make-uniform-policy"
  (prove:is (make-uniform-policy (make-nth-test-game 2)) '(1/4 1/4 1/4 1/4))
  (prove:is (make-uniform-policy (make-nth-test-game 5)) '(1/7 1/7 1/7 1/7 1/7 1/7 1/7))
  (prove:is (make-uniform-policy (make-nth-test-game 100)) nil))

(prove:subtest "Test decide-move-by-random"
  (let* ((game (make-nth-test-game 2))
	 (moves (make-moves game)))
    (labels ((test-decision (rand-val answer-idx)
	       (prove:is (decide-move-by-random-policy game #'make-uniform-policy rand-val)
			 (get-nth-move moves answer-idx))))
      (print moves)
      (test-decision -2 0)
      (test-decision 0 0)
      (test-decision 0.1 0)
      (test-decision 0.25 0)
      (test-decision 0.26 1)
      (test-decision 1 3)
      (test-decision 2 3))))

(prove:subtest "Test move-by-random-policy"
  (defparameter *test-game* (init-game))
  (dotimes (n 10)
    (move-by-random-policy *test-game* #'make-uniform-policy))
  (prove:is (get-game-depth *test-game*) 10))
  
(prove:finalize)
