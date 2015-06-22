(prove:plan 4)

(load "TEST/test-util.lisp")

(prove:subtest "Test make-uniform-policy"
  (labels ((test (depth expected)
	     (let* ((game (make-nth-test-game depth))
		    (move-store (make-moves game)))
	       (prove:is (make-uniform-policy game move-store) expected))))
  (test 2 '(1/4 1/4 1/4 1/4))
  (test 5 '(1/7 1/7 1/7 1/7 1/7 1/7 1/7))
  (test 100 nil)))

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

(prove:subtest "Test prob-store"
  (let ((store (make-prob-store)))
    (prove:subtest "Test initialization"
      (prove:is (prob-store-count store) 0)
      (let ((result t))
	(dotimes (i (length (prob-store-probs store)))
	  (unless (numberp (aref (prob-store-probs store) i))
	    (setf result nil)
	    (return)))
	(prove:ok result "The type of all elements should be number")))
    
    (prove:subtest "Test add ,reset and loop"
      (prove:is-type (add-to-prob-store store 0.1) 'prob-store)
      (prove:is-type (add-to-prob-store store 1/7) 'prob-store)
      (let ((lst nil))
	(do-prob-store (prob store)
	  (setf lst (cons prob lst)))
	(prove:is lst '(1/7 0.1) :test #'equalp))
      
      (prove:is-type (reset-prob-store store) 'prob-store)
      (prove:is (prob-store-count store) 0))
    
    (prove:subtest "Test get-nth-prob"
      (reset-prob-store store)
      (add-to-prob-store store 0.5)
      (prove:ok (not (get-nth-prob nil store)))
      (prove:ok (not (get-nth-prob -1 store)))
      (prove:ok (not (get-nth-prob 1 store)))
      (prove:is (get-nth-prob 0 store) 0.5))))
  
(prove:finalize)
