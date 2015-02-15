(defun make-a-move (x y)
  `(,x . ,y))

(defun add-move (moves x y)
  (cons (make-a-move x y) moves))

(defun get-nth-move (moves n)
  (cond ((null moves) nil)
	((< n 0) nil)
	((= n 0) (car moves))
	(t (get-nth-move (cdr moves) (- n 1)))))

(defun moves-len (moves)
  (length moves))

(defun mapcar-moves (func moves)
  (mapcar func moves))
