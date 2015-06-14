(defun make-a-move (x y)
  `(,x . ,y))

(defun set-to-move (move x y)
  (setf (car move) x)
  (setf (cdr move) y)
  move)

(defun add-move (moves x y)
  (cons (make-a-move x y) moves))

; TODO: make-test
(defmacro move-x (move)
  `(car ,move))
(defmacro move-y (move)
  `(cdr ,move))

(defun move-is-in-board (move)
  (let ((x (car move))
	(y (cdr move)))
    (if (and (not (null x)) (not (null y))
	     (>= x 0) (< x *board-size*) (>= y 0) (< y *board-size*))
	t
	nil)))

(defparameter fns-replace-by-next (make-array 8))
(defmacro make-fn-replace-by-next-move (x-diff y-diff)
  `#'(lambda (move)
       ,(when (neq x-diff 0)
	      `(incf (car move) ,x-diff))
       ,(when (neq y-diff 0)
	      `(incf (cdr move) ,y-diff))
       (values move (move-is-in-board move))))
(labels ((set-fn (dir x-diff y-diff)
	   (setf (aref fns-replace-by-next dir)
		 (make-fn-replace-by-next-move x-diff y-diff))))
  (set-fn *dir-up*          0 -1)
  (set-fn *dir-down*        0  1)
  (set-fn *dir-left*       -1  0)
  (set-fn *dir-right*       1  0)
  (set-fn *dir-left-up*    -1 -1)
  (set-fn *dir-left-down*  -1  1)
  (set-fn *dir-right-up*    1 -1)
  (set-fn *dir-right-down*  1  1))

(defun get-fn-replace-by-next (dir)
  (aref fns-replace-by-next dir))

; TODO: change the order of arguments (to equalize with 'nth')
; TODO: change from func to the macro
(defun get-nth-move (moves n)
  (cond ((null moves) nil)
	((< n 0) nil)
	((= n 0) (car moves))
	(t (get-nth-move (cdr moves) (- n 1)))))

(defun moves-len (moves)
  (length moves))

(defun mapcar-moves (func moves)
  (mapcar func moves))
