; TODO: reduce memory allocation by making a store of probability as move-store
(defun make-uniform-policy(game move-store)
  (let* ((len (move-store-count move-store)))
    (mapcar-move-store (lambda (move) (/ 1 len)) move-store)))

(defun decide-move-by-random-policy(game fn-make-policy rand-val)
  (if (is-game-end game) (return-from decide-move-by-random-policy))
  (let* ((move-store (make-moves game))
	 (policy (funcall fn-make-policy game move-store)))
    (labels ((decide (count sum rest-policy)
	       (cond ((null (car rest-policy)) count)
		     ((>= sum rand-val) count)
		     (t (decide (+ count 1) (+ sum (car rest-policy))
				(cdr rest-policy))))))
      (get-nth-move move-store (decide 0 (car policy) (cdr policy))))))

(defun move-by-random-policy (game fn-make-policy)
  (let ((move (decide-move-by-random-policy game fn-make-policy (random 1.0) )))
    (move-game game (move-x move) (move-y move))))

; --------------------- ;

(defstruct prob-store
  (count 0)
  (probs (make-array *max-move-store* :element-type 'number)))

(defun reset-prob-store (store)
  (setf (prob-store-count store) 0)
  store)

(defun add-to-prob-store (store prob)
  (setf (aref (prob-store-probs store) (prob-store-count store)) prob)
  (incf (prob-store-count store))
  store)

(defmacro do-prob-store (name<>store &body body)
  (let ((i (gensym))
	(g-store (gensym)))
    `(let ((,g-store ,(cadr name<>store)))
       (dotimes (,i (prob-store-count ,g-store))
	 (let ((,(car name<>store) (aref (prob-store-probs ,g-store) ,i)))
	   ,@body)))))

(defmacro get-nth-prob (n store)
  `(cond ((null ,n) nil)
	 ((< ,n 0)  nil)
	 ((>= ,n (prob-store-count ,store)) nil)
	 (t (aref (prob-store-probs ,store) ,n))))
