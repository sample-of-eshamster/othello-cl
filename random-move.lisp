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

; --------------------- ;

(defun make-uniform-policy(game move-store prob-store)
  (declare (ignore game))
  (reset-prob-store prob-store)
  (let* ((len (move-store-count move-store)))
    (do-move-store (move move-store)
      (declare (ignore move))
      (add-to-prob-store prob-store (/ 1 len))))
  prob-store)

(defun decide-move-by-random-policy(game fn-make-policy rand-val prob-store)
  (if (is-game-end game) (return-from decide-move-by-random-policy))
  (let* ((move-store (make-moves game)))
    (labels ((decide (count sum store)
	       (if (>= count (prob-store-count store)) (return-from decide (1- count)))
	       (let ((prob (get-nth-prob count store)))
		 (cond ((null prob) count)
		       ((>= sum rand-val) count)
		       (t (decide (1+ count) (+ sum prob) store))))))
      (funcall fn-make-policy game move-store prob-store)
      (get-nth-move (decide 0 (get-nth-prob 0 prob-store) prob-store) move-store))))

(defun move-by-random-policy (game fn-make-policy &key (prob-store (make-prob-store)))
  (let ((move (decide-move-by-random-policy game fn-make-policy (random 1.0) prob-store)))
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
