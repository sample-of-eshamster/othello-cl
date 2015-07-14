(prove:plan 7)

(load "TEST/test-util.lisp")

; TODO: make *ucb-max* const in source
(defparameter *expected-ucb-max* 10000)
(prove:subtest
    "Test calc-ucb"
  (prove:ok (> (calc-ucb 2  0 10) *expected-ucb-max*))
  (prove:ok (> (calc-ucb 2 -2 10) *expected-ucb-max*))
  (prove:ok (> (calc-ucb 2 10  0) *expected-ucb-max*))
  (prove:ok (> (calc-ucb 2 10 -2) *expected-ucb-max*))
  (prove:ok (> (calc-ucb 2 -2 -2) *expected-ucb-max*))
  (prove:ok (< (calc-ucb -2 10 10) *expected-ucb-max*))

  (prove:ok (< (calc-ucb 2 2 2)
	       (calc-ucb 4 2 2)))
  (prove:ok (< (calc-ucb 2 2 2)
	       (calc-ucb 2 1 2)))
  (prove:ok (< (calc-ucb 2 2 2)
	       (calc-ucb 2 2 4)))
  
  (prove:is (calc-ucb -2 10 10 :turn *white*)
	    (calc-ucb 2 10 10 :turn *black*))
  (prove:is (calc-ucb 2 10 10 :turn *white*)
	    (calc-ucb -2 10 10 :turn *black*))
  (prove:is (calc-ucb -2 0 10 :turn *white*)
	    (calc-ucb -2 0 10 :turn *black*))

  (within (calc-ucb 2 2 8) 2.4420 0.0001)
  (within (calc-ucb -2 2 8) 0.4420 0.0001))


(defparameter *end-status-list* `(,*white* ,*black* ,*empty*))
(defun prove-game-is-end (game)
  (prove-in (get-game-result game) *end-status-list*))
(prove:subtest
    "Test sim-to-game-end"
  (let ((prob-store (make-prob-store)))
    (prove-game-is-end (sim-to-game-end (init-game) #'make-uniform-policy prob-store))
    (prove-game-is-end (sim-to-game-end (make-nth-test-game 3) #'make-uniform-policy prob-store))
    (prove-game-is-end (sim-to-game-end (make-nth-test-game 100)  #'make-uniform-policy prob-store))))

(prove:subtest
    "Test mc-simulate-once"
  (defun prove-mc-sim-once (start repeat)
    (let ((game (make-nth-test-game start)))
      (dotimes (x repeat)
	(prove-in (mc-simulate-once game #'make-uniform-policy) *end-status-list*))
      (prove:ok (is-game-same-phase game (make-nth-test-game start)))))
  (prove-mc-sim-once 3 5)
  (prove-mc-sim-once 4 5))

(prove:subtest
    "Test init-mc-nodes"
  (prove:is (init-mc-nodes (make-nth-test-game 3)) '(#S(MC-NODE :MOVE (2 . 4) :SUM 0 :NUM 0) #S(MC-NODE :MOVE (4 . 2) :SUM 0 :NUM 0)) :test #'equalp)
  (defun prove-mc-node-len (start)
    (let ((game (make-nth-test-game start)))
      (prove:is (length (init-mc-nodes game)) (move-store-count (make-moves game)))))
  (prove-mc-node-len 5)
  (prove-mc-node-len 34)
  (prove-mc-node-len 100))

(prove:subtest
    "Test select-mc-node-by-ucb"
  (defun prove-all-node-selected-in-ucb (start)
    (let* ((game (make-nth-test-game start))
	   (mc-node (init-mc-nodes game))
	   (len (length mc-node)))
      (when (<= len 0) (return-from prove-all-node-selected-in-ucb t))
      (dotimes (x len)
	(incf (mc-node-num (select-mc-node-by-ucb mc-node x)) 1))
      (prove:ok (every #'(lambda(node) (= (mc-node-num node) 1)) mc-node))))
  (prove-all-node-selected-in-ucb 2)
  (prove-all-node-selected-in-ucb 3)

  (prove:pass "I have no plan to validate the selection by select-mc-node-by-ucb"))


(defmacro t-nth-mc-num (n nodes)
  `(mc-node-num (nth ,n ,nodes)))
(defmacro t-nth-mc-sum (n nodes)
  `(mc-node-sum (nth ,n ,nodes)))
(prove:subtest
    "Test select-mc-node-by-ave"
  (let ((test-mc-nodes (init-mc-nodes (make-nth-test-game 57))))
    (assert (= (length test-mc-nodes) 3))
    
    (setf (t-nth-mc-sum 0 test-mc-nodes) -2)
    (setf (t-nth-mc-num 0 test-mc-nodes) 5)
    (setf (t-nth-mc-sum 2 test-mc-nodes) -7)
    (setf (t-nth-mc-num 2 test-mc-nodes) 10)
    (prove:is (mc-node-sum (select-mc-node-by-ave test-mc-nodes)) -2)
    
    (setf (t-nth-mc-sum 2 test-mc-nodes) 3)
    (prove:is (mc-node-sum (select-mc-node-by-ave test-mc-nodes)) 3)))
  
(prove:subtest
    "Test mc-simulate"
  (prove:ok (not (mc-simulate (make-nth-test-game 100) #'make-uniform-policy 5)))

  (defun prove-mc-simulate (start)
    (let* ((game (make-nth-test-game start))
	   (move (mc-simulate game #'make-uniform-policy 20)))
      (prove:ok (is-game-same-phase game (make-nth-test-game start)))
      (prove:ok (check-move-valid (game-board game) (car move) (cdr move) (game-turn game)))))

  (prove-mc-simulate 13)
  (prove-mc-simulate 24))

(prove:finalize)
