(defstruct game
  board
  turn
  move-store
  history)

(defun init-game()
  (let ((a-game (make-game :board (init-board)
			   :turn 1
			   :move-store (init-move-store)
			   :history (make-history-record-store))))
    a-game))

(defun is-game-end(game)
  (let ((turn (game-turn game)))
    (and (neq turn *white*) (neq turn *black*))))

; TODO: compare by hash
(defun is-game-same-phase(game1 game2)
  (and
   (eq (game-turn game1) (game-turn game2))
   (equalp (game-board game1) (game-board game2))))

(defun judge-next-turn (moved-game)
  (let* ((board (game-board moved-game))
	 (turn (game-turn moved-game))
	 (rev-turn (reverse-turn turn)))
    (if (< 0 (move-store-count (make-moves-on-board board rev-turn (game-move-store moved-game))))
	rev-turn
	(if (< 0 (move-store-count (make-moves-on-board board turn (game-move-store moved-game))))
	    turn
	    0))))

(defun move-game (game x y)
  (let ((board (game-board game))
	(turn (game-turn game)))
    (unless (check-move-valid board x y turn)
      (return-from move-game nil))
    (regist-new-history-record
     (game-history game)
     #'(lambda (record)
	 (block exit
	   (unless (move-on-board (game-board game) x y turn
				  :reverse-store (history-record-reverse-list record))
	     (return-from exit nil))
	   (set-to-move (history-record-move record) x y)
	   (setf (history-record-turn record) turn)
	   t)))
    (setf (game-turn game) (judge-next-turn game))
    game))

(defun reverse-game (game)
  (if (<= (get-game-depth game) 0) (return-from reverse-game nil))
  (let* ((record (pop-history-record (game-history game)))
	 (move (history-record-move record)))
    (set-to-board (game-board game) (car move) (cdr move) *empty*)
    (do-move-store (move (history-record-reverse-list record))
      (let ((x (move-x move))
	    (y (move-y move)))
	(set-to-board (game-board game) x y
		      (reverse-turn (get-piece (game-board game) x y)))))
    (setf (game-turn game) (history-record-turn record))
    game))

(defun reverse-game-to-depth (game depth)
  (if (<= (get-game-depth game) (max 0 depth))
      (return-from reverse-game-to-depth game))
  (reverse-game game)
  (reverse-game-to-depth game depth))

(defun get-game-depth (game)
  (history-record-store-count (game-history game)))

(defun make-moves (game)
  (make-moves-on-board (game-board game) (game-turn game) (game-move-store game)))

(defmacro do-in-move-reverse (game move &body body)
  (let ((result (gensym))
	(g-game (gensym))
	(g-move (gensym)))
    `(let ((,g-game ,game)
	   (,g-move ,move))
       (unless (check-move-valid
		(game-board ,g-game) (car ,g-move) (cdr ,g-move) (game-turn ,g-game))
	   (error (format nil "ERROR: An Invalid Move! (~A)" ,g-move)))
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
  (let ((moves nil))
    (do-move-store (move (make-moves game))
      (setf moves (cons move moves)))
    (print `(Move-> ,moves)))
  (if prints-history
      (labels ((print-a-history (history)
		 (let ((lst nil))
		   (do-move-store (move (history-record-reverse-list history))
		     (setf lst (cons move lst)))
		   (format t "TURN: ~2D, Move: ~A, REVERSE-LIST: ~D~%"
			   (history-record-turn history)
			   (history-record-move history)
			   lst))))
	(fresh-line)
	(princ "+++++ history start +++++")
	(fresh-line)
	(do-history-record-store (record (game-history game))
	  (print-a-history record))
	(princ "+++++ history end +++++")))
  (fresh-line))
 
