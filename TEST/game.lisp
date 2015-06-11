(prove:plan 8)

(load "TEST/test-util.lisp")

(defparameter *test-game* (init-game))

(prove:subtest "Test init-game"
  (prove:is *test-game* #S(GAME :BOARD #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :TURN 1 :HISTORY NIL)
	    :test #'equalp)
  (prove:is (get-game-depth *test-game*) 0)
  (prove:ok (not (reverse-game *test-game*))))
  
(prove:subtest "Test move-game"
  (prove:ok (not (move-game *test-game* -1 3)))
  (prove:ok (not (move-game *test-game* 5 5))))

(prove:subtest "Test is-game-same-phase"
  (prove:ok (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 3)))
  (prove:ok (is-game-same-phase (make-nth-test-game 0) (init-game)))
  (prove:ok (not (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 4))))
  (prove:ok (not (is-game-same-phase
		(move-game (make-nth-test-game 2) 4 5)
		(move-game (make-nth-test-game 2) 1 2)))))

(prove:subtest "Test funcs for test"
  (setf *test-game* (make-nth-test-game 3))

  (defparameter *expected-game* #S(GAME :BOARD #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) :TURN -1 :HISTORY (#S(HISTORY-RECORD :TURN 1 :MOVE (2 . 3) :REVERSE-LIST ((3 . 3))) #S(HISTORY-RECORD :TURN -1 :MOVE (2 . 2) :REVERSE-LIST ((3 . 3))) #S(HISTORY-RECORD :TURN 1 :MOVE (3 . 2) :REVERSE-LIST ((3 . 3))))))

  (prove:ok (is-game-same-phase *test-game* *expected-game*))
  (prove:is (get-game-depth *test-game*) 3)
  (prove:is (make-moves *test-game*) '((2 . 4) (4 . 2))))

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
  (prove:is-print (print-game (make-nth-test-game 3) t)
		  "   01234567
|0 -------- |
|1 -------- |
|2 --XO---- |
|3 --OOO--- |
|4 ---OX--- |
|5 -------- |
|6 -------- |
|7 -------- |
Black turn

(MOVE-> ((2 . 4) (4 . 2))) 
+++++ history start +++++
#S(HISTORY-RECORD :TURN 1 :MOVE (2 . 3) :REVERSE-LIST ((3 . 3)))
#S(HISTORY-RECORD :TURN -1 :MOVE (2 . 2) :REVERSE-LIST ((3 . 3)))
#S(HISTORY-RECORD :TURN 1 :MOVE (3 . 2) :REVERSE-LIST ((3 . 3)))
+++++ history end +++++
"))

(prove:finalize)
