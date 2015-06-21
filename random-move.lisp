; TODO: reduce memory allocation by making a store of probability as move-store
; TODO: receive move-store from caller
(defun make-uniform-policy(game)
  (let* ((move-store (make-moves game))
	 (len (move-store-count move-store)))
    (mapcar-move-store (lambda (move) (/ 1 len)) move-store)))

(defun decide-move-by-random-policy(game fn-make-policy rand-val)
  (if (is-game-end game) (return-from decide-move-by-random-policy))
  (let* ((move-store (make-moves game))
	 (policy (funcall fn-make-policy game)))
    (labels ((decide (count sum rest-policy)
	       (cond ((null (car rest-policy)) count)
		     ((>= sum rand-val) count)
		     (t (decide (+ count 1) (+ sum (car rest-policy))
				(cdr rest-policy))))))
      (get-nth-move move-store (decide 0 (car policy) (cdr policy))))))

(defun move-by-random-policy (game fn-make-policy)
  (let ((move (decide-move-by-random-policy game fn-make-policy (random 1.0) )))
    (move-game game (move-x move) (move-y move))))
