; --------- human --------- ;

(defun move-by-human(game)
  (print-game game nil)
  (if (is-game-end game)
      (progn (print "This game has ended")
	     (return-from move-by-human))
  (loop while (not (eval-play-command game (read-command))))))

; --------- minimax --------- ;
(defun move-by-minimax(game depth)
  (if (is-game-end game)
      (return-from move-by-minimax))
  (let ((move (select-move-by-minimax game depth #'eval-game-by-ab)))
    (move-game game (car move) (cdr move))))

; --------- random --------- ;

(defun move-by-uniform-random(game)
  (if (is-game-end game)
      (return-from move-by-uniform-random))
  (move-by-random-policy game #'make-uniform-policy))

(defun move-by-uniform-mc(game times)
  (if (is-game-end game)
      (return-from move-by-uniform-mc))
  (let ((move (mc-simulate game #'make-uniform-policy times)))
    (move-game game (car move) (cdr move))))

(defun move-by-default-uct(game times)
  (if (is-game-end game)
      (return-from move-by-default-uct))
  (let ((move (uct-simulate game (make-def-uct-param) times)))
    (move-game game (car move) (cdr move))))
