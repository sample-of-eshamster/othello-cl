(defun sim-to-game-end (game fn-make-policy)
  (if (is-game-end game)
      game
      (progn (move-by-random-policy game fn-make-policy)
	     (sim-to-game-end game fn-make-policy))))
  
; This can be implemented using do-in-move-reverse macro,
; but I'm afraid that the implementation doesn't be a tail call
(defun mc-simulate-once (game fn-make-policy)
  (let ((depth (get-game-depth game)))
      (let ((result (get-game-result (sim-to-game-end game fn-make-policy))))
	(reverse-game-to-depth game depth)
	result)))

; use UCB
(defstruct mc-node
  move
  sum
  num)

(defun calc-ucb (sum num total-num &key (coef 1.41421356) (turn *white*))
  (if (and (< 0 num) (< 0 total-num))
      (* (+ (/ sum num) (* coef (sqrt (/ (log total-num) num))))
	 (if (= turn *white*) 1 -1))
      99999))

(defun init-mc-nodes (game)
  (let ((moves (make-moves game)))
    (mapcar-moves (lambda (move)
		    (make-mc-node :move move
				  :sum 0
				  :num 0))
		  moves)))

(defun select-mc-node-by-ucb (mc-nodes total-num)
  (select-max-node (lambda (node)
		     (calc-ucb (mc-node-sum node) (mc-node-num node) total-num))
		   mc-nodes))

(defun select-mc-node-by-ave (mc-nodes)
  (select-max-node (lambda (node)
		     (let ((num (mc-node-num node))
			   (sum (mc-node-sum node)))
		       (if (< 0 num)
			   (/ sum num)
			   -99999)))
		   mc-nodes))

(defun mc-simulate (game fn-make-policy times)
  (let ((mc-nodes (init-mc-nodes game)))
    (if (null mc-nodes) (return-from mc-simulate nil))
    (dotimes (now-times times)
      (let* ((node (select-mc-node-by-ucb mc-nodes now-times))
	     (move (mc-node-move node))
	     (turn (game-turn game)))
	(do-in-move-reverse game move
	  (let ((result (mc-simulate-once game fn-make-policy)))
	    (cond ((= result turn) (incf (mc-node-sum node)))
		  ((= result (reverse-turn turn)) (decf (mc-node-sum node)))))
	  (incf (mc-node-num node)))))
    (mc-node-move (select-mc-node-by-ave mc-nodes))))
