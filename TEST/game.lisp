(prove:plan 8)

(load "TEST/test-util.lisp")

(defparameter *test-game* (init-game))

(prove:subtest "Test init-game"
  (prove:is (get-game-depth *test-game*) 0)
  (prove:ok (not (reverse-game *test-game*))))
  
(prove:subtest "Test move-game"
  (prove:ok (not (move-game *test-game* -1 3)))
  (prove:ok (not (move-game *test-game* 5 5)))
  (let* ((game (make-nth-test-game 2))
	 (depth (get-game-depth game)))
    (assert (move-game game 4 5))
    (prove:is (get-game-depth game) (1+ depth))))

(prove:subtest "Test is-game-same-phase"
  (prove:ok (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 3)))
  (prove:ok (is-game-same-phase (make-nth-test-game 0) (init-game)))
  (prove:ok (not (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 4))))
  (prove:ok (not (is-game-same-phase
		(move-game (make-nth-test-game 2) 4 5)
		(move-game (make-nth-test-game 2) 1 2)))))

(prove:subtest "Test funcs for test"
  (setf *test-game* (make-nth-test-game 3))

  (prove:is (get-game-depth *test-game*) 3)
  (let ((store (make-moves *test-game*)))
    (prove:is (move-store-count store) 2)
    (prove:ok (contains-move store 2 4))
    (prove:ok (contains-move store 4 2))))

(prove:subtest "Test reverse-game"
  (setf *test-game* (make-nth-test-game 5))
  (reverse-game *test-game*)
  (reverse-game *test-game*)

  (prove:ok (is-game-same-phase *test-game* (make-nth-test-game 3)))

  (prove:ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 3)
				(make-nth-test-game 3)))
  (prove:ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 0)
				(init-game)))
  (prove:ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 8)
				(make-nth-test-game 5)))
  (prove:ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) -2)
				(init-game))))
(prove:subtest "Test funcs about the game end"
  (prove:ok (not (is-game-end (init-game))))
  (prove:ok (not (is-game-end (make-nth-test-game 3))))
  (prove:ok (is-game-end (make-nth-test-game 100)))

  (prove:is (get-game-result (init-game)) *not-game-end*)
  (prove:is (get-game-result (make-nth-test-game 3)) *not-game-end*)

  (prove:is (get-game-result (make-nth-test-game 100)) *black*)
  (let ((num-cell (* *board-size* *board-size*)))
    (labels ((test (white black expected)
	       (let ((game (init-game)))
		 (dotimes (i num-cell)
		   (set-to-board (game-board game)
				 (mod i *board-size*)
				 (floor (/ i *board-size*))
				 (if (< i white) *white*
				     (if (< i (+ white black)) *black* *empty*))))
		 (setf (game-turn game) *empty*)
		 (assert (is-game-end game))
		 (prove:is (get-game-result game) expected))))
      (test 33 31 *white*)
      (test 31 25 *white*)
      (test 31 33 *black*)
      (test 25 31 *black*)
      (test 30 30 *empty*)
      (test 32 32 *empty*))))


(prove:subtest "Test do-in-move-reverse"
  (setf *test-game* (make-nth-test-game 3))
  (prove:is (do-in-move-reverse *test-game* (make-a-move 2 4)
	      (let ((test-par 2))
		(* (get-game-depth *test-game*) test-par))) 8)
  (prove:is (do-in-move-reverse *test-game* (make-a-move 2 4)
	      (do-in-move-reverse *test-game* (make-a-move 5 5)
		(get-game-depth *test-game*)))
	    5)
  (prove:is-error (do-in-move-reverse *test-game* (make-a-move 5 5)
		    (print 'empty))
		  'simple-error)
  (prove:is (get-game-depth *test-game*) 3))

(prove:subtest "Test print-game"
  (prove:is-print (print-game (make-nth-test-game 4) t)
		  "   01234567
|0 -------- |
|1 -------- |
|2 --XO---- |
|3 --XOO--- |
|4 --XXX--- |
|5 -------- |
|6 -------- |
|7 -------- |
White turn

(MOVE-> ((5 . 5) (4 . 5) (3 . 5) (2 . 5) (1 . 5) (1 . 4) (1 . 3) (1 . 2) (1 . 1))) 
+++++ history start +++++
TURN: -1, Move: (2 . 4), REVERSE-LIST: ((3 . 4) (2 . 3))
TURN:  1, Move: (2 . 3), REVERSE-LIST: ((3 . 3))
TURN: -1, Move: (2 . 2), REVERSE-LIST: ((3 . 3))
TURN:  1, Move: (3 . 2), REVERSE-LIST: ((3 . 3))
+++++ history end +++++
"))

(prove:finalize)
