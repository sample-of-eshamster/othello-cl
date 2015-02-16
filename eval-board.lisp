(defparameter *corner-moves*
  (list (make-a-move 0 0)
	(make-a-move 0 (- *board-size* 1))
	(make-a-move (- *board-size* 1) 0)
	(make-a-move (- *board-size* 1) (- *board-size* 1))))

(defparameter *def-eval-param* (make-hash-table))
(setf (gethash 'corner *def-eval-param*) 4)
(setf (gethash 'num-move *def-eval-param*) 1)

(defun eval-game-static (game turn &optional (param *def-eval-param*))
  (if (is-game-end game)
      (return-from eval-game-static
	(let ((game-result (get-game-result game)))
	 (cond ((eq game-result turn) 100000)
	       ((eq game-result (reverse-turn turn)) -100000)
	       (t -50000)))))
  (let ((board (game-board game)))
    (labels ((eval-corner ()
	       (let ((score 0))
		 (dolist (corner *corner-moves*)
		   (let ((piece (get-piece board (car corner) (cdr corner)))
			 (base-score (gethash 'corner param)))
		     (cond ((eq piece turn) (incf score base-score))
			   ((eq piece (reverse-turn turn)) (decf score base-score)))))
		 score))
	     (eval-num-move ()
	       (* (length (make-moves game))
		  (if (eq turn (game-turn game)) 1 -1)
		  (gethash 'num-move param))))
      (+ (eval-corner)
	 (eval-num-move)))))