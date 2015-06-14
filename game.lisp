; ----------- history-record -------------
(defstruct history-record
  turn
  move
  reverse-list)

; ----------- game -------------
(defstruct game
  board
  turn
  history)

(defun init-game()
  (let ((a-game (make-game :board (init-board)
			   :turn 1
			   :history nil)))
    a-game))

(defun is-game-end(game)
  (let ((turn (game-turn game)))
    (and (neq turn *white*) (neq turn *black*))))

; TODO: compare by hash
(defun is-game-same-phase(game1 game2)
  (equalp game1 game2))

(defun judge-next-turn (moved-game)
  (let* ((board (game-board moved-game))
	 (turn (game-turn moved-game))
	 (rev-turn (reverse-turn turn)))
    (if (< 0 (length (make-moves-on-board board rev-turn)))
	rev-turn
	(if (< 0 (length (make-moves-on-board board turn)))
	    turn
	    0))))

(defun move-game (game x y)
  (let ((board (game-board game))
	(turn (game-turn game))
	(reverse-list nil))
    (if (check-move-valid board x y turn)
	(progn (setf reverse-list
		     (move-on-board (game-board game) x y turn))
	       (setf (game-history game)
		     (cons (make-history-record :turn turn
						:move (cons x y)
						:reverse-list reverse-list)
			   (game-history game)))
	       (setf (game-turn game) (judge-next-turn game))
	       game)
	nil)))

(defun reverse-game (game)
  (if (not (game-history game)) (return-from reverse-game nil))
  (let* ((record (car (game-history game)))
	 (move (history-record-move record)))
    (set-to-board (game-board game) (car move) (cdr move) *empty*)
    (dolist (pnt (history-record-reverse-list record))
      (let ((x (car pnt))
	    (y (cdr pnt)))
	(set-to-board (game-board game) x y
		      (reverse-turn (get-piece (game-board game) x y)))))
    (setf (game-turn game) (history-record-turn record))
    (setf (game-history game) (cdr (game-history game)))
    game))

(defun reverse-game-to-depth (game depth)
  (if (<= (get-game-depth game) (max 0 depth))
      (return-from reverse-game-to-depth game))
  (reverse-game game)
  (reverse-game-to-depth game depth))

(defun get-game-depth (game)
  (length (game-history game)))

(defun make-moves (game)
  (make-moves-on-board (game-board game) (game-turn game)))

(defmacro do-in-move-reverse (game move &body body)
  (let ((result (gensym))
	(g-game (gensym))
	(g-move (gensym)))
    `(let ((,g-game ,game)
	   (,g-move ,move))
       (if (not (check-move-valid
		 (game-board ,g-game) (car ,g-move) (cdr ,g-move) (game-turn ,g-game)))
	   (error "ERROR: An Invalid Move!"))
       (move-game ,g-game (car ,g-move) (cdr ,g-move))
       (let ((,result (progn ,@body)))
	 (reverse-game ,g-game)
	 ,result))))

(defun print-turn (turn)
  (cond ((eq turn *white*) (princ "White turn"))
	((eq turn *black*) (princ "Black turn"))
	(t (princ "Game Finished")))
  (fresh-line))

(defun get-game-result (game)
  (if (not (is-game-end game)) (return-from get-game-result *not-game-end*))
  (let ((white (count-piece (game-board game) *white*))
	(black (count-piece (game-board game) *black*)))
    (cond ((> white black) *white*)
	  ((< white black) *black*)
	  (t *empty*))))

(defun print-game (game &optional (prints-history nil))
  (print-board (game-board game))
  (print-turn (game-turn game))
  (print `(Move-> ,(make-moves game)))
  (if prints-history
      (labels ((print-a-history (history)
		 (princ history)
		 (fresh-line)))
	(fresh-line)
	(princ "+++++ history start +++++")
	(fresh-line)
	(mapcar #'print-a-history (game-history game))
	(princ "+++++ history end +++++")))
  (fresh-line))
 
